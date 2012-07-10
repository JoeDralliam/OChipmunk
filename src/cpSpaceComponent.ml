open CpType
open CpPrivate


let activate_body space body =
  assert (not (Body.is_rogue body)) ;
  if space.splocked <> 0
  then begin
    if not (List.memq body space.sproused_bodies)
    then space.sproused_bodies <- body :: space.sproused_bodies
  end
  else begin
    space.spbodies <- body :: space.spbodies ;
    Body.foreach_shape body (fun shape -> begin
      CpSpatialIndex.remove space.spstatic_shapes shape Int64.(of_int shape.shhashid) ;
      CpSpatialIndex.insert space.spactive_shapes shape Int64.(of_int shape.shhashid) ;
    end) ;

    Body.foreach_arbiter body (fun arb -> begin
      let body_a = arb.abody_a in
      if Body.eql body body_a || Body.is_static body_a
      then begin
        let contacts = arb.acontacts in
        let num_contacts = CpArray.length contacts in
        arb.acontacts <- CpContactBuffer.get_array space num_contacts ;
        CpArray.memcpy arb.acontacts contacts ;
        CpContactBuffer.push_contacts space num_contacts ;

        let a = arb.aa and b = arb.ab in
        let shape_pair = (a,b) in
        let arb_hashid = hash_pair (a.shhashid) (b.shhashid) in
        ignore( CpHashSet.insert space.spcached_arbiters arb_hashid shape_pair (fun _ x -> x) arb ) ;

        arb.astamp <- space.spstamp ;
        arb.ahandler <- Space.lookup_handler space a.shcollision_type b.shcollision_type ;
        space.sparbiters <- arb :: space.sparbiters 
      end
    end ) ;

    Body.foreach_constraint body (fun constr ->
      let body_a = constr.ca in
      if Body.eql body body_a || Body.is_static body_a
      then space.spconstraints <- constr :: space.spconstraints
    )
  end

let deactivate_body space body =
  assert (not (Body.is_rogue body)) ;

  space.spbodies <- List.filter (fun b -> b != body) space.spbodies ;

  Body.foreach_shape body (fun shape -> CpSpatialIndex.(
    remove space.spactive_shapes shape Int64.(of_int shape.shhashid) ;
    insert space.spstatic_shapes shape Int64.(of_int shape.shhashid)
  ) ) ;

  Body.foreach_arbiter body (fun arb -> begin
    let body_a = arb.abody_a in
    if Body.eql body body_a || Body.is_static body_a
    then begin
      Space.uncache_arbiter space arb ;
      arb.acontacts <- CpArray.copy arb.acontacts ;
    end
  end) ;

  Body.foreach_constraint body (fun constr ->
    let body_a = constr.ca in
    if Body.eql body body_a || Body.is_static body_a
    then space.spconstraints <- List.filter (fun c -> c != constr) space.spconstraints
  )

module Component =
struct
  let root b =
    match b with
      | Some body -> ComponentNode.(body.bnode.root)
      | None -> None

  let activate r =
    CpOption.do_if (fun root ->
      if not (Body.is_sleeping root)
      then begin
        assert (not (Body.is_rogue root)) ;
        let space = CpOption.(!?(root.bspace)) in
        let rec impl b = 
          CpOption.do_if ComponentNode.(fun body ->
            let next = body.bnode.next in
            body.bnode.idle_time <- 0. ;
            body.bnode.root <- None ;
            body.bnode.next <- None ;
            activate_body space body ;
            impl next
          ) b
        in impl (Some root) ;
        space.spsleeping_components <- List.filter (fun b -> b != root) space.spsleeping_components
      end 
    ) r

  let add root body =
    ComponentNode.(body.bnode.root <- Some root) ;
    if body != root
    then ComponentNode.(
      body.bnode.next <- root.bnode.next ;
      root.bnode.next <- Some body
    )

  let rec flood_fill rt body =
    if not (Body.is_rogue body)
    then
      match root (Some body) with
        | None -> begin
          add rt body ;
          Body.foreach_arbiter body (fun arb -> flood_fill rt (if Body.eql body arb.abody_a then arb.abody_b else arb.abody_a)) ;
          Body.foreach_constraint body (fun constr -> flood_fill rt (if Body.eql body constr.ca then constr.cb else constr.ca)) ;
        end
        | Some other_rt -> if other_rt != rt then Printf.eprintf "Internal Error : Inconstency detexted in the contact graph"
            
  let active root threshold =
    let rec impl b = 
      match b with
        | Some body -> ComponentNode.(
            let next = body.bnode.next in
            if body.bnode.idle_time < threshold
            then true
            else impl next
        )
        | None -> false
    in impl root
end
