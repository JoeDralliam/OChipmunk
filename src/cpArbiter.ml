open CpType

type t = arbiter'

let get_elasticity arb = arb.ae
let set_elasticity arb e' = arb.ae <- e'

let get_friction arb = arb.au
let set_friction arb u' = arb.au <- u'

let get_surface_velocity arb = arb.asurface_vr
let set_surface_velocity arb v' = arb.asurface_vr <- v'

let get_count arb =
  if arb.astate <> Cached then CpArray.length arb.acontacts else 0


let total_impulse arb =
  let do_sum sum contact =
    CpVector.(add sum CpContact.(mult contact.n contact.jn_acc))
  in
  let sum = 
    if get_count arb <> 0
    then CpArray.fold_left do_sum CpVector.zero arb.acontacts
    else CpVector.zero 
  in
  if arb.aswapped_coll then sum else CpVector.neg sum
    
let total_impulse_with_friction arb =
  let do_sum sum contact = 
    CpVector.add sum CpContact.(CpVector.rotate contact.n (CpVector.make contact.jn_acc contact.jt_acc))
  in
  let sum = 
    if get_count arb <> 0
    then CpArray.fold_left do_sum CpVector.zero arb.acontacts
    else CpVector.zero 
  in
  if arb.aswapped_coll then sum else CpVector.neg sum

let total_ke arb =
  let e_coef = (1. -. arb.ae) /. (1. +. arb.ae) in
  let do_sum sum contact =
    let jn_acc = CpContact.(contact.jn_acc) in
    let jt_acc = CpContact.(contact.jt_acc) in
    let t_mass = CpContact.(contact.t_mass) in
    let n_mass = CpContact.(contact.n_mass) in
    sum +. e_coef*.jn_acc*.jn_acc/.n_mass +. jt_acc*.jt_acc/.t_mass
  in
  if get_count arb <> 0
  then CpArray.fold_left do_sum 0. arb.acontacts
  else 0.

let ignore arb =
  arb.astate <- Ignore

let make a b =
  assert (a.shbody != b.shbody) ;
  {
    ahandler = None ;
    aswapped_coll = false ;

    ae = 0. ;
    au = 0. ;
    asurface_vr = CpVector.zero ;

    acontacts = CpArray.make [||] 0 0;

    aa = a ; abody_a = a.shbody ;
    ab = b ; abody_b = b.shbody ;

    athread_a = { thnext = None ; thprev = None } ;
    athread_b = { thnext = None ; thprev = None } ;

    astamp = 0 ;
    astate = FirstColl
  }

let get_shapes arb =
  if arb.aswapped_coll
  then (arb.ab,arb.aa)
  else (arb.aa,arb.ab)

let get_bodies arb =
  let (s_a, s_b) = get_shapes arb in
  (s_a.shbody, s_b.shbody)

let get_contact_point_set arb =
  let count = get_count arb in
  let open ContactPointSet in
      let contact_point_info i =
	{
	  point  = CpContact.((CpArray.get arb.acontacts i).p) ;
	  normal = CpContact.((CpArray.get arb.acontacts i).n) ;
	  dist   = CpContact.((CpArray.get arb.acontacts i).dist)
	}
      in
      { count = count ; points = Array.init count contact_point_info }

let is_first_contact arb =
  arb.astate = FirstColl

let get_normal arb i =
  let open ContactPointSet in
      assert (0 <= i && i < (get_count arb)) ;
      let n = CpContact.((CpArray.get arb.acontacts i).n) in
      if arb.aswapped_coll then CpVector.neg n else n
	

let get_point arb i =
  let open ContactPointSet in
      assert (0 <= i && i < (get_count arb)) ;
      CpContact.((CpArray.get arb.acontacts i).p)

let get_depth arb i =
  let open ContactPointSet in
      assert (0 <= i && i < (get_count arb)) ;
      CpContact.((CpArray.get arb.acontacts i).dist)


  (* PRIVATE *)
let next arb body =
  if CpPrivate.Body.eql arb.abody_a body then arb.athread_a.thnext else arb.athread_b.thnext

let call_separate arb space =
  let handler = CpPrivate.Space.lookup_handler space arb.aa.shcollision_type arb.ab.shcollision_type in
  CpOption.(!?handler).chseparate arb space


let unthread_helper arb body =
  let thread = CpArbiterThread.for_body arb body in
  let prev = thread.thprev in
  let next = thread.thnext in

  (match prev with
    | Some p -> (CpArbiterThread.for_body p body).thnext <- next
    | None -> body.barbiter_list <- next );
  
  (match next with 
    | Some n -> (CpArbiterThread.for_body n body).thprev <- prev
    | None -> () ) ;
  thread.thnext <- None ;
  thread.thprev <- None


let unthread arb = 
  unthread_helper arb arb.abody_a ;
  unthread_helper arb arb.abody_b

let update arb contacts' handler' a' b' =
  let look_for_hash_value_match old =
    let modify_new_contact new_contact = CpContact.(
      if new_contact.hash = old.hash
      then begin
	new_contact.jn_acc <- old.jn_acc ;
        new_contact.jt_acc <- old.jt_acc
      end
    ) in
    CpArray.iter modify_new_contact contacts'
  in
  CpArray.iter look_for_hash_value_match arb.acontacts ;
  arb.acontacts <- contacts' ;
  arb.ahandler <- Some handler' ;
  arb.aswapped_coll <- (a'.shcollision_type <> handler'.cha) ;
  
  arb.ae <- a'.she *. b'.she ;
  arb.au <- a'.shu *. b'.shu ;
  arb.asurface_vr <- CpVector.sub a'.shsurface_v b'.shsurface_v ;

  arb.aa <- a' ; arb.abody_a <- a'.shbody ;
  arb.ab <- b' ; arb.abody_b <- b'.shbody ;

  if arb.astate = Cached then arb.astate <- FirstColl

let prestep arb dt slop bias =
  let a = arb.abody_a in
  let b = arb.abody_b in

  CpArray.iter (fun con -> CpContact.(
    con.r1 <- CpVector.sub con.p a.bp ;
    con.r2 <- CpVector.sub con.p b.bp ;
    
    con.n_mass <- 1. /. (CpConstraintUtils.k_scalar a b con.r1 con.r2 con.n) ;
    con.t_mass <- 1. /. (CpConstraintUtils.k_scalar a b con.r1 con.r2 (CpVector.perp con.n)) ;
    
    con.bias <- -.bias *. (min 0. (con.dist +. slop)) /. dt ;
    con.j_bias <- 0. ;
    
    con.bounce <- (CpConstraintUtils.normal_relative_velocity a b con.r1 con.r2 con.n) *. arb.ae
  ) ) arb.acontacts

let apply_cached_impulse arb dt_coef =
  if (not (is_first_contact arb))
  then 
    let a = arb.abody_a in
    let b = arb.abody_b in
    CpArray.iter (fun con -> CpContact.(
      let j = CpVector.rotate con.n (CpVector.make con.jn_acc con.jt_acc) in
      CpConstraintUtils.apply_impulses a b con.r1 con.r2 (CpVector.mult j dt_coef)
    )) arb.acontacts

let apply_impulse arb =
  let a = arb.abody_a in
  let b = arb.abody_b in
  let surface_vr = arb.asurface_vr in
  let friction = arb.au in
  CpArray.iter (fun con -> CpContact.(
    let n_mass = con.n_mass in
    let n = con.n in
    let r1 = con.r1 in
    let r2 = con.r2 in

    let vb1 = CpVector.(add a.bv_bias (mult (perp r1) a.bw_bias)) in
    let vb2 = CpVector.(add b.bv_bias (mult (perp r2) b.bw_bias)) in
    let vr = CpConstraintUtils.relative_velocity a b r1 r2 in

    let vbn = CpVector.(dot (sub vb2 vb1) n) in
    let vrn = CpVector.dot vr n in
    let vrt = CpVector.(dot (add vr surface_vr) (perp n)) in

    let jbn = (con.bias -. vbn) *. n_mass in
    let jbn_old = con.j_bias in
    con.j_bias <- max (jbn_old +. jbn) 0. ;

    let jn = -.(con.bounce +. vrn) *. n_mass in
    let jn_old = con.jn_acc in
    con.jn_acc <- max (jn_old +. jn) 0. ;

    let jt_max = friction *. con.jn_acc in
    let jt = -.vrt *. con.t_mass in
    let jt_old = con.jt_acc in
    con.jt_acc <- CpFloat.clamp (jt_old +. jt) (-.jt_max) (jt_max) ;

    CpConstraintUtils.apply_bias_impulses a b r1 r2 (CpVector.mult n (con.j_bias -. jbn_old)) ;
    CpConstraintUtils.apply_impulses a b r1 r2 CpVector.(rotate n (make (con.jn_acc -. jn_old) (con.jt_acc -. jt_old)))
  )) arb.acontacts
