open CpType

type t = arbiter'

let get_elasticity arb = arb.ae
let set_elasticity arb e' = arb.ae <- e'

let get_friction arb = arb.au
let set_friction arb u' = arb.au <- u'

let get_surface_velocity arb = arb.asurface_vr
let set_surface_velocity arb v' = arb.asurface_vr <- v'

let total_impulse arb =
  let do_sum sum contact =
    CpVector.(add sum CpContact.(mult contact.n contact.jn_acc))
  in
  let sum = CpArray.fold_left do_sum CpVector.zero arb.acontacts in
  if arb.aswapped_coll then sum else CpVector.neg sum
    
let total_impulse_with_friction arb =
  let do_sum sum contact = 
    CpVector.add sum CpContact.(CpVector.rotate contact.n (CpVector.make contact.jn_acc contact.jt_acc))
  in
  let sum = CpArray.fold_left do_sum CpVector.zero arb.acontacts in
  if arb.aswapped_coll then sum else CpVector.neg sum

let total_ke arb =
  let e_coef = (1. -. arb.ae) /. (1. +. arb.ae) in
  let do_sum sum contact =
    let jn_acc = CpContact.(contact.jn_acc) in
    let jt_acc = CpContact.(contact.jt_acc) in
    let t_mass = CpContact.(contact.t_mass) in
    let n_mass = CpContact.(contact.n_mass) in
    e_coef*.jn_acc*.jn_acc/.n_mass +. jt_acc*.jt_acc/.t_mass
  in
  CpArray.fold_left do_sum 0. arb.acontacts

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
  let count = CpArray.length arb.acontacts in
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

let get_count arb =
  if arb.astate <> Cached then CpArray.length arb.acontacts else 0

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
  if arb.abody_a == body then arb.athread_a.thnext else arb.athread_b.thnext

let call_separate arb space =
  let handler = CpPrivate.Space.lookup_handler space arb.aa.shcollision_type arb.ab.shcollision_type in
  CpOption.(!?handler).chseparate arb space


let unthread_helper arb body =
  let thread = CpArbiterThread.for_body arb body in
  let prev = thread.thprev in
  let next = thread.thnext in

  (match prev with
    | Some arb -> (CpArbiterThread.for_body arb body).thnext <- next
    | None -> body.barbiter_list <- next );
  
  (match next with 
    | Some arb -> (CpArbiterThread.for_body arb body).thprev <- prev
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



(*
  type state = FirstColl | Normal | Ignore | Cached



  module ContactPointSet =
  struct
  type u = { point:CpVector.t ; normal:CpVector.t ; dist:float }
  type t = { count:int ; points:u array }
  end


  class arbiter arb a' b' =
  object
  val mutable e = 0. 
  val mutable u = 0.
  val mutable surface_vr = CpVector.zero

  val mutable a = a'
  val mutable b = b'
  val mutable body_a = a'#get_body
  val mutable body_b = b'#get_body

  val thread_a = { prev = None ; next = None }
  val thread_b = { prev = None ; next = None }

  val contacts = [||]

  val mutable stamp = 0
  val mutable handler =  None
  val mutable swapped_coll = false
  val mutable state = FirstColl

  method get_elasticity = e
  method set_elasticity e' = e <- e'

  method get_friction = u
  method set_friction u' = u <- u'

  method get_surface_velocity = surface_vr
  method set_surface_velocity v' = surface_vr <- v'

  method total_impulse =
  let do_sum sum contact =
  CpVector.(add sum CpContact.(mult contact.n contact.jn_acc))
  in
  let sum = Array.fold_left do_sum CpVector.zero contacts in
  if swapped_coll then sum else CpVector.neg sum
  
  method total_impulse_with_friction =
  let do_sum sum contact =
  CpVector.(add sum CpContact.(rotate contact.n (make contact.jn_acc contact.jt_acc))
  in
  let sum = Array.fold_left do_sum CpVector.zero contacts in
  if swapped_coll then sum else CpVector.neg sum

  method total_ke =
  let e_coef = (1. -. e) /. (1. +. e) in
  let do_sum sum contact =
  let jn_acc = CpContact.(contact.jn_acc) in
  let jt_acc = CpContact.(contact.jt_acc) in
  let t_mass = CpContact.(contact.t_mass) in
  let n_mass = CpContact.(contact.n_mass) in
  e_coef*.jn_acc*.jn_acc/.n_mass +. jt_acc*.jt_acc/.t_mass
  in
  Array.fold_left do_sum 0. contacts

  method ignore =
  state <- StateIgnore

  method get_shapes =
  if swapped_coll
  then (b,a)
  else (a,b)

  method get_bodies =
  let (s_a, s_b) = self#get_shapes in
  (s_a#get_body, s_b#get_body)

  method get_contact_point_set =
  let open ContactPointSet in
  let contact_point_info i =
  {
  point = CpContact.(contacts.(i).p) ;
  normal = CpContact.(contacts.(i).n) ;
  dist = CpContact.(contacts.(i).dist)
  }
  in
  { count = count ; points = Array.init contact_point_info count }

  method is_first_contact =
  state = StateFirstColl

  method get_count =
  if state <> StateCached then Array.length contacts else 0

  method get_normal i =
  let open ContactPointSet in
  assert (0 <= i && i <= self#get_count) ;
  let n = CpContact.(contacts.(i).n) in
  if swapped_coll then CpVector.neg n else n
  

  method get_point i =
  let open ContactPointSet in
  assert (0 <= i && i <= self#get_count) ;
  CpContact.(contacts.(i).p)

  method get_depth i =
  let open ContactPointSet in
  assert (0 <= i && i <= self#get_count) ;
  CpContact.(contacts.(i).dist)


(* PRIVATE *)
  method next body =
  if body_a = body then thread_a#next else thread_b#next

  method call_separate space =
  let handler = space#lookup_handler space a#get_collision_type b#get_collision_type in
  handler.separate self space

  method thread_for_body body =
  if a#body_a = body then thread_a else thread_b

  method unthread = 
  unthread_helper body_a ;
  unthread_helper body_b

  method private unthread_helper body =
  let thread = CpArbiterThread.for_body self body in
  let prev = thread#prev in
  let next = thread#next in

  (match prev with
  | Some arb -> (CpArbiterThread.for_body arb body)#set_next next
  | None -> body#set_arbiter_list next );
  
  (match next with 
  | Some arb -> (CpArbiterThread.for_body arb body)#set_next next
  | None -> () ) ;
  thread#set_next None ;
  thread#set_prev None

  method update contacts' handler' a' b' =
  let look_for_hash_value_match old =
  let modify_new_contact new_contact =
  if new_contact.hash = old.hash
  then CpContact.(
  new_contact.jn_acc <- old.jn_acc ;
  new_contact.jt_acc <- old.jt_acc
  )
  in
  Array.iter modify_new_contact contacts'
  in
  Array.iter look_for_hash_value_match contacts ;
  contacts <- contacts' ;
  handler <- handler' ;
  swapped_coll <- (a'#collision_type <> CpCollisionHandler.(handler'.a)) ;
  
  e <- a'#get_elasticity *. b'#get_elasticity ;
  u <- a'#get_friction *. b'#get_friction ;
  surface_vr <- CpVector.(sub a'#get_surface_velocity b'#get_surface_velocity) ;

  a <- a' ; body_a <- a'#get_body ;
  b <- b' ; body_b <- b'#get_body ;

  if state = Cached then state <- FirstColl

  method pre_step dt slop bias =
  Array.iter (fun con -> CpContact.(
  con.r1 <- CpVector.sub con.p body_a#get_pos ;
  con.r2 <- CpVector.sub con.p body_b#get_pos ;
  
  con.n_mass <- 1. / (k_scalar body_a body_b con.r1 con.r2 con.n) ;
  con.t_mass <- 1. / (k_scalar body_a body_b con.r1 con.r2 (CpVector.perp con.n)) ;
  
  con.bias <- -.bias *. (min 0. (con.dist +. slop)) /. dt ;
  con.j_bias <- 0. ;
  
  con.bounce <- (normal_relative_velocity body_a body_b con.r1 con.r2 con.n) *. e
  ) ) contacts

  method apply_cached_impulse dt_coef =
  if (not arb#is_first_contact)
  then Array.iter (fun con -> CpContact.(
  let j = CpVector.rotate con.n (CpVector.make con.jn_acc con.jt_acc) in
  apply_impulses body_a body_b con.r1 con.r2 (CpVector.mult j dt_coef)
  )) contacts

  method apply_impulse =
  let a = body_a in
  let b = body_b in
  Array.iter (fun con -> CpContact.(
  let n_mass = con.n_mass in
  let n = con.n in
  let r1 = con.r1 in
  let r2 = con.r2 in

  let vb1 = CpVector.(add a#get_v_bias (mult (perp r1) a#get_v_bias)) in
  let vb2 = CpVector.(add b#get_v_bias (mult (perp r2) b#get_v_bias)) in
  let vr = relative_velocity a b r1 r2 in

  let vbn = CpVector.(dot (sub vb2 vb1) n) in
  let vrn = CpVector.dot vr n in
  let vrt = CpVector.(dot (add vr surface_vr) (perp n)) in

  let jbn = (con.bias -. vbn) *. n_mass in
  let jbn_old = con.j_bias in
  con.j_bias <- max (jbn_old +. jbn) 0. ;

  let jn = -.(con.bounce +. vrn) *. n_mass in
  let jn_old = con.jn_acc in
  con.jn_acc <- max (jn_old +. jn) 0. ;

  let jt_max = u *. con.jn_acc in
  let jt = -.vrt *. con.t_mass in
  let jt_old = con.jt_acc in
  con.jt_acc <- clamp (jt_old +. jt) (-jt_max) (jt_max) ;

  apply_bias_impulses a b r1 r2 (CpVector.mult n (con.j_bias -. jbn_old)) ;
  apply_impulses a b r1 r2 CpVector.(rotate n (make (con.jn_acc -. jn_old) (con.jt_acc -. jt_old)))
  )) contacts
  end
*)          
