open CpType
type t = body'

let do_if f a =
  match a with 
    | Some q -> f q 
    | None -> ()

let is_sleeping = CpPrivate.Body.is_sleeping

let is_static = CpPrivate.Body.is_static
  
let is_rogue = CpPrivate.Body.is_rogue

let kinetic_energy = CpPrivate.Body.kinetic_energy

let eql = CpPrivate.Body.eql

let activate body =
  if not (is_rogue body)
  then begin
    ComponentNode.(body.bnode.idle_time <- 0.);
    CpSpaceComponent.Component.(activate (root (Some body)))
  end

let activate_static body filter =
  assert (is_static body) ;

  let activate_arbiter arb =
    activate (if eql arb.abody_a body then arb.abody_b else arb.abody_a)
  in

  CpPrivate.Body.foreach_arbiter body (fun arb ->
    match filter with
      | None -> activate_arbiter arb
      | Some filt when CpPrivate.Shape.eql filt arb.aa || CpPrivate.Shape.eql filt arb.ab 
          -> activate_arbiter arb
      | _ -> ()
  )

let push_arbiter body arb =
  CpPrivate.assert_soft ((CpArbiterThread.for_body arb body).thnext = None) "Internal Error: Dangling contact graph pointers detected. (A)" ;
  CpPrivate.assert_soft ((CpArbiterThread.for_body arb body).thprev = None) "Internal Error: Dangling contact graph pointers detected. (B)" ;

  let next = body.barbiter_list in
  CpPrivate.assert_soft (next = None || (CpArbiterThread.for_body CpOption.(!?next) body).thprev = None) "Internal Error: Dangling contact graph pointers detected. (C)" ; 
  (CpArbiterThread.for_body arb body).thnext <- next ;

  CpOption.do_if (fun n -> (CpArbiterThread.for_body n body).thprev <- Some arb) next ;
  body.barbiter_list <- Some arb

let sleep_with_group body group =
  assert (not (is_static body) && not (is_rogue body)) ;

  let space = CpOption.(!?(body.bspace)) in
  assert (space.splocked <> 0) ;
  assert (group = None || is_sleeping CpOption.(!?group))  ;
  if is_sleeping body
  then assert  CpSpaceComponent.Component.( 
    match (root (Some body), root group) with 
      | (None,None) -> true
      | (Some a, Some b) when eql a b -> true
      | _ -> false
  ) ;

  CpPrivate.Body.foreach_shape body (fun shape -> CpPrivate.Shape.update shape body.bp body.brot) ;
  CpSpaceComponent.deactivate_body space body ;

  if group <> None
  then ComponentNode.(
    let root = CpSpaceComponent.Component.root group in
    body.bnode.root <- root ;
    body.bnode.next <- CpOption.(!?root).bnode.next ;
    body.bnode.idle_time <- 0.
  )
  else ComponentNode.(
    body.bnode.root <- Some body ;
    body.bnode.next <- None ;
    body.bnode.idle_time <- 0. ;

    space.spsleeping_components <- body :: space.spsleeping_components
  ) ;

  space.spbodies <- List.filter (fun b -> b != body ) space.spbodies


let sleep body = sleep_with_group body None

let sanity_check body =
  CpPrivate.assert_soft (body.bm = body.bm && body.bm_inv = body.bm_inv) "Body's mass is invalid." ;
  CpPrivate.assert_soft (body.bi = body.bi && body.bi_inv = body.bi_inv) "Body's moment is invalid." ;

  CpVector.assert_sane body.bp "Body's position is invalid." ;
  CpVector.assert_sane body.bv "Body's velocity is invalid." ;
  CpVector.assert_sane body.bf "Body's force is invalid" ;
  
  CpPrivate.assert_soft (body.ba = body.ba && (abs_float body.ba) <> infinity) "Body's angle is invalid." ;    
  CpPrivate.assert_soft (body.bw = body.bw && (abs_float body.bw) <> infinity) "Body's angular velocity is invalid." ;
  CpPrivate.assert_soft (body.bt = body.bt && (abs_float body.bt) <> infinity) "Body's torque is invalid." ;

  CpVector.assert_sane body.brot "Body's rotation vector is invalid" ;

  CpPrivate.assert_soft (body.bv_limit = body.bv_limit) "Body's velocity limit is invalid" ;
  CpPrivate.assert_soft (body.bw_limit = body.bw_limit) "Body's angular velocity limit is invalid"

    
let assert_sane = sanity_check

let get_mass body = body.bm
let set_mass body mass =
  assert (mass > 0.) ;
  activate body ;
  body.bm <- mass ;
  body.bm_inv <- 1. /. mass

let get_moment body = body.bi
let set_moment body moment =
  assert (moment > 0.);
  activate body ;
  body.bi <- moment ;
  body.bi_inv <- 1. /. moment

let get_pos body = body.bp

let get_vel body = body.bv
let set_vel body v = 
  activate body ;
  assert_sane body ;
  body.bv <- v

let get_force body = body.bf
let set_force body v = 
  activate body ;
  assert_sane body ;
  body.bf <- v

let get_angle body = body.ba

let get_ang_vel body = body.bw
let set_ang_vel body v = 
  activate body ;
  assert_sane body ;
  body.bw <- v

let get_torque body = body.bt
let set_torque body v = 
  activate body ;
  assert_sane body ;
  body.bt <- v

let get_rot body = body.brot

let get_vel_limit body = body.bv_limit
let set_vel body v = 
  activate body ;
  assert_sane body ;
  body.bv_limit <- v

let get_vel body = body.bv
let set_vel body v = 
  activate body ;
  assert_sane body ;
  body.bv <- v

let get_ang_vel_limit body = body.bw_limit
let set_ang_vel_limit body v = 
  activate body ;
  assert_sane body ;
  body.bw_limit <- v

let local2world body v =
  CpVector.(add body.bp (rotate v body.brot))

let world2local body v =
  CpVector.(unrotate (sub v body.bp) body.brot)

let add_shape body shape =
  let next = body.bshape_list in
  (match next with
    | Some n -> n.shprev <- (Some shape)
    | _ -> () ) ;
  shape.shnext <- next ;
  body.bshape_list <- (Some shape)

let remove_shape body shape =
  let prev = shape.shprev in
  let next = shape.shnext in
  (match prev with
    | Some p -> p.shnext <- next
    | None -> body.bshape_list <- next) ;

  (match next with
    | Some n -> n.shprev <- prev
    | _ -> () ) ;

  shape.shprev <- None ;
  shape.shnext <- None


let rec filter_constraints n body filter =
  match n with 
    | Some node ->
        if CpConstraint.eql node filter
        then CpPrivate.next_constraint node body
        else begin
          if eql node.ca body
          then node.cnext_a <- (filter_constraints node.cnext_a body filter)
          else node.cnext_b <- (filter_constraints node.cnext_b body filter) ;
          n
        end
    | None -> failwith "Constraint not found (CpBody.filter_constraint)"

let remove_constraint body constraint' =
  body.bconstraint_list <- filter_constraints body.bconstraint_list body constraint'

let set_pos body pos =
  activate body ;
  assert_sane body ;
  body.bp <- pos

let set_angle_helper body angle =
  body.ba <- angle ;
  body.brot <- CpVector.forangle angle

let set_angle body angle =
  activate body ;
  assert_sane body ;
  set_angle_helper body angle

let update_velocity body gravity damping dt =
  body.bv <- CpVector.(clamp (add (mult body.bv damping) (mult (add gravity (mult body.bf body.bm_inv)) dt)) body.bv_limit) ;

  let w_limit = body.bw_limit in
  body.bw <- CpFloat.clamp ( body.bw *. damping +. body.bt *. body.bi_inv *. dt) (-.w_limit) (w_limit) ;

  sanity_check body

let update_position body dt =
  body.bp <- CpVector.(add body.bp (mult (add body.bv body.bv_bias) dt)) ;
  set_angle body (body.ba +. (body.bw +. body.bw_bias)*.dt) ;

  body.bv_bias <- CpVector.zero ;
  body.bw_bias <- 0. ;

  sanity_check body

let reset_forces body =
  activate body ;
  body.bf <- CpVector.zero ;
  body.bt <- 0.

let apply_force body force r =
  activate body ;
  body.bf <- CpVector.add body.bf force ;
  body.bt <- body.bt +. CpVector.(cross r force)

let apply_impulse body j r =
  activate body ;
  CpConstraintUtils.apply_impulse body j r

let get_vel_at_point body r =
  CpVector.(add body.bv (mult (perp r) body.bw))

let get_vel_at_world_point body point =
  get_vel_at_point body CpVector.(sub point body.bp)

let get_vel_at_local_point body point =
  get_vel_at_point body CpVector.(rotate point body.brot)

let each_shape body func =
  let rec iter list =
    match list with
      | Some shape -> begin
        let next = shape.shnext in
        func body shape ;
        iter next
      end
      | None -> ()
  in iter body.bshape_list


let each_constraint body func =
  let rec iter list =
    match list with
      | Some constr -> begin
        let next = CpPrivate.next_constraint constr body in
        func body constr ;
        iter next
      end
      | None -> ()
  in iter body.bconstraint_list

let each_arbiter body func =
  let rec iter list =
    match list with
      | Some arb -> begin
        let next = CpPrivate.next_arbiter arb body in
        arb.aswapped_coll <- (eql body arb.abody_b) ;
        func body arb ;
        iter next
      end
      | None -> ()
  in iter body.barbiter_list

let make m i =
  let body = {
    bvelocity_func = update_velocity ;
    bposition_func = update_position ;
    
    bm = 0. ;
    bm_inv = 0.;
    bi = 0. ;
    bi_inv = 0.;
    
    bp = CpVector.zero ;
    bv = CpVector.zero ;
    bf = CpVector.zero ;
    
    ba = 0. ;
    bw = 0. ;
    bt = 0. ;
    
    brot = CpVector.zero ;
    
    
    bv_limit = infinity ;
    bw_limit = infinity ;
    
    bv_bias = CpVector.zero ;
    bw_bias = 0. ;
    
    bspace = None ;

    bshape_list = None ;
    barbiter_list = None ;
    bconstraint_list = None ;
    

    bnode = ComponentNode.({ root = None ; next = None ; idle_time = 0. })
  } in
  set_mass body m ;
  set_moment body i ;
  set_angle body 0. ;
  body

let make_static () =
  let body = make infinity infinity in
  ComponentNode.(body.bnode.idle_time <- infinity) ;
  body


