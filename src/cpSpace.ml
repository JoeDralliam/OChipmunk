open CpType

type t = space'

let get_if opt def =
  match opt with
    | Some v -> v
    | None -> def


let default_collision_handler =
  {
    cha = 0 ; chb = 0 ;
    chbegin = (fun _ _ -> true) ;
    chpresolve = (fun _ _ -> true) ;
    chpostsolve = (fun _ _ -> ()) ;
    chseparate = (fun _ _ -> ())
  }


module ArbiterSet =
struct
  let eql (a,b) arb =
    (a == arb.aa && b == arb.ab) || (b == arb.aa && a == arb.ab)

  let trans (a,b) space =
    CpArbiter.make a b

  let filter space arb =
    let ticks = space.spstamp - arb.astamp in
    let a = arb.abody_a and b = arb.abody_b in
    if CpBody.((is_static a || is_sleeping a) && (is_static b || is_sleeping b))
    then true
    else begin
      if ticks >= 1 && arb.astate <> Cached
      then (arb.astate <- Cached ; CpArbiter.call_separate arb space) ;

      if ticks >= space.spcollision_persistence
      then (arb.acontacts <- CpArray.make [||] 0 0 ; false)
      else true
    end
end

module HandlerSet =
struct
  let eql pair check =
    (check.cha == (fst pair) && check.chb == (snd pair)) || (check.chb == (fst pair) && check.cha == (snd pair))

  let trans pair (beg,presol,postsol,sep) =
    { 
      cha = fst pair ;
      chb = snd pair ;
      chbegin = beg  ;
      chpresolve = presol ;
      chpostsolve = postsol ;
      chseparate = sep ;
    }
      
  let pair a b =
    { default_collision_handler with cha = a ; chb = b }
end

let make () =
  let spstatic_shapes = CpSpatialIndex.make_bb_tree CpShape.get_bb None in
  let spactive_shapes = 
    let tree = CpSpatialIndex.BBTree.make () in
    CpSpatialIndex.BBTree.set_velocity_func tree (fun sh -> sh.shbody.bv) ;
    CpSpatialIndex.make (CpSpatialIndexType.BBTree tree) CpShape.get_bb (Some spstatic_shapes) 
  in
  let space = {
    spcontact_buffers_head = None ;

    spiterations = 10 ;

    spgravity = CpVector.zero ;
    spdamping = 1. ;

    spcollision_slop = 0.1 ;
    spcollision_bias = (1.0 -. 0.1) ** 60.;
    spcollision_persistence = 3 ;

    splocked = 0 ;
    spstamp = 0 ;


    spstatic_shapes ;
    spactive_shapes ;

    spbodies = [] ;
    spsleeping_components = [] ;
    sproused_bodies = [] ;

    spsleep_time_threshold = infinity ;
    spidle_speed_threshold = 0. ;
    spenable_contact_graph = false ;

    sparbiters = [] ;

(*    spcontact_buffers_head = None ; *)
    spcached_arbiters = CpHashSet.make 0 ArbiterSet.eql ;

    spconstraints = [] ;
    
    spdefault_handler = default_collision_handler ;
    spcollision_handlers = CpHashSet.make 0 HandlerSet.eql ;

    sppost_step_callbacks = [] ;
    spskip_post_step = false ;

    spstatic_body = CpBody.make_static ()  ;

    spcurr_dt = 0.
  }
  in
  CpHashSet.set_default_value space.spcollision_handlers (Some default_collision_handler) ;
  space

let get_iterations space = space.spiterations
let set_iterations space v = space.spiterations <- v
let get_gravity space = space.spgravity
let set_gravity space v = space.spgravity <- v
let get_damping space = space.spdamping
let set_damping space v = space.spdamping <- v
let get_idle_speed_threshold space = space.spidle_speed_threshold
let set_idle_speed_threshold space v = space.spidle_speed_threshold <- v
let get_sleep_time_threshold space = space.spsleep_time_threshold
let set_sleep_time_threshold space v = space.spsleep_time_threshold <- v
let get_collision_slop space = space.spcollision_slop
let set_collision_slop space v = space.spcollision_slop <- v
let get_collision_bias space = space.spcollision_bias
let set_collision_bias space v = space.spcollision_bias <- v
let get_collision_persistence space = space.spcollision_persistence
let set_collision_persistence space v = space.spcollision_persistence <- v
let get_enable_contact_graph space = space.spenable_contact_graph
let set_enable_contact_graph space v = space.spenable_contact_graph <- v
let get_static_body space = space.spstatic_body
let get_current_time_step space = space.spcurr_dt


let lookup_handler = CpPrivate.Space.lookup_handler

let activate_body space body = ()

let is_locked space = space.splocked <> 0

let assert_unlocked space = assert (space.splocked = 0)

let lock space = space.splocked <- space.splocked + 1

let unlock space run_post_step =
  space.splocked <- space.splocked - 1 ;
  assert (space.splocked >= 0) ;
  
  if space.splocked = 0 && run_post_step && not space.spskip_post_step
  then begin
    space.spskip_post_step <- true ;
    List.iter (CpSpaceComponent.activate_body space) space.sproused_bodies ;
    space.sproused_bodies <- [] ;

    List.iter (fun (_, callback) -> callback space ) space.sppost_step_callbacks ;
    space.sppost_step_callbacks <- [] ;
    space.spskip_post_step <- false
  end

let set_default_collision_handler space ?begin' ?presolve ?postsolve ?separate () = 
  assert_unlocked space;
  let handler = {
    cha = 0 ; chb = 0 ;
    chbegin = get_if begin' (fun _ _ -> true) ;
    chpresolve = get_if presolve (fun _ _ -> true) ;
    chpostsolve = get_if postsolve (fun _ _ -> ()) ;
    chseparate = get_if separate (fun _ _ -> ())
  } in
  space.spdefault_handler <- handler ;
  CpHashSet.set_default_value space.spcollision_handlers (Some space.spdefault_handler)
    

let remove_collision_handler space a b =
  assert_unlocked space ;
  CpHashSet.remove space.spcollision_handlers (CpPrivate.hash_pair a b) (a,b)


let add_collision_handler space a b ?begin' ?presolve ?postsolve ?separate () =
  assert_unlocked space;
  ignore( remove_collision_handler space a b );
  let handler_funcs = (
    get_if begin' (fun _ _ -> true) ,
    get_if presolve (fun _ _ -> true) ,
    get_if postsolve (fun _ _ -> ()) ,
    get_if separate (fun _ _ -> () )
  ) in
  CpHashSet.insert space.spcollision_handlers (CpPrivate.hash_pair a b) (a,b) HandlerSet.trans (handler_funcs)

let add_static_shape space shape =
  assert (shape.shspace = None) ;
  assert_unlocked space;
  let body = shape.shbody in
  CpBody.add_shape body shape ;
  CpShape.update shape body.bp body.brot ;
  CpSpatialIndex.insert space.spstatic_shapes shape (Int64.of_int shape.shhashid) ;
  shape.shspace <- Some space ;
  shape
    
let add_shape space shape =
  let body = shape.shbody in
  if CpBody.is_static body
  then add_static_shape space shape
  else begin 
    assert (shape.shspace = None) ;
    assert_unlocked space ;

    CpBody.activate body ;
    CpBody.add_shape body shape ;
    CpShape.update shape body.bp body.brot ;
    CpSpatialIndex.insert space.spactive_shapes shape (Int64.of_int shape.shhashid) ;
    shape.shspace <- Some space ;
    shape
  end

let add_body space body =
  assert (not (CpBody.is_static body)) ;
  assert (body.bspace = None) ;
  assert_unlocked space ;
  space.spbodies <- body :: space.spbodies ;
  body.bspace <- Some space ;
  body

let add_constraint space constraint' =
  assert (constraint'.cspace = None) ;
  assert_unlocked space ;

  CpBody.activate constraint'.ca ;
  CpBody.activate constraint'.cb ;
  space.spconstraints <- constraint':: space.spconstraints ;

  let a = constraint'.ca and b = constraint'.cb in
  constraint'.cnext_a <- a.bconstraint_list ; a.bconstraint_list <- Some constraint' ;
  constraint'.cnext_b <- b.bconstraint_list ; b.bconstraint_list <- Some constraint' ;
  constraint'.cspace <- Some space ;
  constraint'


let contains_shape space shape =
  CpOption.(shape.shspace ==? space)

let contains_body space body =
  CpOption.(body.bspace ==? space)

let contains_constraint space constraint' =
  CpOption.(constraint'.cspace ==? space)


let filter_arbiters space body shape =
  let filter arb =
    let match_shape other = 
      match shape with 
	| Some s  -> s == other
	| None -> true
    in
    if (body == arb.abody_a && (match_shape arb.aa)) ||
      (body == arb.abody_b && (match_shape arb.ab))
    then begin
      if shape <> None && arb.astate <> Cached
      then CpArbiter.call_separate arb space ;
      CpArbiter.unthread arb ;
      space.sparbiters <- List.filter (fun a -> a != arb) space.sparbiters ;
      false
    end
    else true
  in	
  lock space ;
  CpHashSet.filter space.spcached_arbiters filter ;
  unlock space true


let remove_static_shape space shape =
  assert (contains_shape space shape) ;
  assert_unlocked space ;
  
  let body = shape.shbody in
  if CpBody.is_static body then CpBody.activate_static body (Some shape) ;
  CpBody.remove_shape body shape ;
  filter_arbiters space body (Some shape) ;
  CpSpatialIndex.remove space.spstatic_shapes shape (Int64.of_int shape.shhashid) ;
  shape.shspace <- None


let remove_shape space shape =
  let body = shape.shbody in
  if CpBody.is_static body 
  then remove_static_shape space shape
  else begin
    assert (contains_shape space shape) ;
    assert_unlocked space ;

    CpBody.activate body ;
    CpBody.remove_shape body shape ;
    filter_arbiters space body (Some shape) ;
    CpSpatialIndex.remove space.spactive_shapes shape (Int64.of_int shape.shhashid) ;
    shape.shspace <- None
  end


let remove_body space body =
  assert (contains_body space body) ;
  assert_unlocked space ;
  CpBody.activate body ;
  space.spbodies <- List.filter (fun b -> b != body) space.spbodies ;
  body.bspace <- None

let remove_constraint space constraint' =
  assert (contains_constraint space constraint') ;
  assert_unlocked space ;
  
  CpBody.activate constraint'.ca ;
  CpBody.activate constraint'.cb ;
  space.spconstraints <- List.filter (fun c -> c != constraint') space.spconstraints ;
  CpBody.remove_constraint constraint'.ca constraint' ;
  CpBody.remove_constraint constraint'.cb constraint' ;
  constraint'.cspace <- None
    

let exist_post_step_callback space key =
  List.exists (fun (k,_) -> k = key ) space.sppost_step_callbacks

let add_post_step_callback space func key =
  if not (exist_post_step_callback space key)
  then 
    let psc = (key, fun space -> func space) in
    space.sppost_step_callbacks <- psc :: space.sppost_step_callbacks
    
let point_query space point layers group func =
  let impl _ shape = 
    if (shape.shgroup = 0 || group <> shape.shgroup) &&
      ((layers land shape.shlayers) <> 0) &&
      ( CpShape.point_query shape point)
    then func shape
  in

  let bb = CpBB.make_for_circle point 0. in
  lock space ;
  CpSpatialIndex.query space.spactive_shapes () bb impl ;
  CpSpatialIndex.query space.spstatic_shapes () bb impl ;
  unlock space true
  
let point_query_first space point layers group =
  let out_shape = ref None in
  point_query space point layers group 
    (fun shape -> if not shape.shsensor then out_shape := Some shape) ;
  !out_shape
  
let nearest_point_query space point max_distance layers group func =
  let impl _ shape =
    if (shape.shgroup = 0 || group <> shape.shgroup) &&
      ((layers land shape.shlayers) <> 0)
    then 
      let (d,info) = CpShape.nearest_point_query shape point in
      if CpNearestPointQueryInfo.(info.shape <> None) && d < max_distance
      then func shape d CpNearestPointQueryInfo.(info.p)
  in
  let bb = CpBB.make_for_circle point (max max_distance 0.) in
  lock space ;
  CpSpatialIndex.query space.spactive_shapes () bb impl ;
  CpSpatialIndex.query space.spstatic_shapes () bb impl ;
  unlock space true
  
let nearest_point_query_nearest space point max_distance layers group =
  let out = ref CpNearestPointQueryInfo.({ shape = None ; p = CpVector.zero ; d = max_distance }) in
  let impl _ shape =
    if (shape.shgroup = 0 || group <> shape.shgroup) &&
      ((layers land shape.shlayers) <> 0)
    then
      let (d,info) = CpShape.nearest_point_query shape point in
      if d < CpNearestPointQueryInfo.(!out.d) then out := info
  in
  let bb = CpBB.make_for_circle point (max max_distance 0.) in
  CpSpatialIndex.query space.spactive_shapes () bb impl ;
  CpSpatialIndex.query space.spstatic_shapes () bb impl ;
  (CpNearestPointQueryInfo.(!out.shape), !out)


let segment_query space start end' layers group func =
  let impl _ shape =
    if (shape.shgroup = 0 || group <> shape.shgroup) &&
      ((layers land shape.shlayers) <> 0)
    then begin
      let (query,info) = CpShape.segment_query shape start end' in
      if query then func shape CpSegmentQueryInfo.(info.t) CpSegmentQueryInfo.(info.n)
    end ;
    1.
  in

  lock space ;
  CpSpatialIndex.segment_query space.spstatic_shapes () start end' 1. impl ;
  CpSpatialIndex.segment_query space.spactive_shapes () start end' 1. impl ;
  unlock space true



let segment_query_first space start end' layers group =
  let out = ref CpSegmentQueryInfo.({ shape = None ; t = 1. ; n = CpVector.zero }) in
  let impl _ shape =
    if (shape.shgroup = 0 || group <> shape.shgroup) &&
      ((layers land shape.shlayers) <> 0) &&
      (not shape.shsensor)
    then begin
      let (query,info) = CpShape.segment_query shape start end' in
      if query && CpSegmentQueryInfo.(info.t < !out.t)
      then out := info
    end ;
    CpSegmentQueryInfo.(!out.t)
  in
  CpSpatialIndex.segment_query space.spstatic_shapes () start end' 1. impl ;
  CpSpatialIndex.segment_query space.spactive_shapes () start end' 1. impl ;
  (CpSegmentQueryInfo.(!out.shape), !out)

let bb_query space bb layers group func =
  let impl _ shape =
    if (shape.shgroup = 0 || group <> shape.shgroup) &&
      ((layers land shape.shlayers) <> 0) &&
      CpBB.(intersects bb shape.shbb)
    then func shape 
  in
  lock space ;
  CpSpatialIndex.query space.spactive_shapes () bb impl ;
  CpSpatialIndex.query space.spstatic_shapes () bb impl ;
  unlock space true

let shape_query space shape func = 
  let any_collision = ref false in
  let impl a b =
    if (a.shgroup = 0 || a.shgroup <> b.shgroup) &&
      ((a.shlayers land b.shlayers) <> 0) &&
      (a != b)
    then begin
      let contacts = 
        let a = Array.init max_contacts_per_arbiter (fun _ -> CpContact.make ()) in
        CpArray.make a 0 max_contacts_per_arbiter
      in
      let num_contacts =
        if CpShape.(get_type a <= get_type b)
        then CpCollision.collide_shapes a b contacts
        else CpContact.(
          let n = CpCollision.collide_shapes b a contacts in
          assert (n < CpArray.length contacts) ;
          for i = 0 to (n-1) do CpArray.(get contacts i).n <- CpVector.(neg CpArray.(get contacts i).n) done ;
          n
        )
      in

      if num_contacts <> 0
      then begin
        any_collision := true ;
        match func with
          | Some f -> ContactPointSet.(
            let set = { 
              count = num_contacts ;
              points = Array.init max_contacts_per_arbiter 
                (fun i -> 
                  if i < num_contacts
                  then  { point  = CpContact.(CpArray.(get contacts i).p) ; 
                          normal = CpContact.(CpArray.(get contacts i).n) ;
                          dist   = CpContact.(CpArray.(get contacts i).dist) }
                  else { point = CpVector.zero ; normal = CpVector.zero ; dist = 0. })
            } in
            f b set
          )
          | None -> ()
      end
    end
  in
  let body = shape.shbody in
  let bb = CpShape.update shape body.bp body.brot in
  lock space ;
  CpSpatialIndex.query space.spactive_shapes shape bb impl ;
  CpSpatialIndex.query space.spstatic_shapes shape bb impl ;
  unlock space true ;
  !any_collision

let activate_shapes_touching_shape space shape =
  if space.spsleep_time_threshold <> infinity
  then ignore (shape_query space shape (Some (fun shp _ -> CpBody.activate shp.shbody)))

  
let each_body space func =
  lock space ;
  List.iter func space.spbodies ;
  List.iter (fun root -> CpPrivate.Body.foreach_component (Some root) func) space.spsleeping_components ;
  unlock space true

let each_shape space func =
  lock space ;
  CpSpatialIndex.each space.spactive_shapes func ;
  CpSpatialIndex.each space.spstatic_shapes func ;
  unlock space true 

let each_constraint space func =
  lock space;
  List.iter func space.spconstraints ;
  unlock space true

let reindex_static space =
  let update_bb_cache shape =
    let body = shape.shbody in
    ignore( CpShape.update shape body.bp body.brot )
  in
  assert (space.splocked = 0) ;
  CpSpatialIndex.each space.spstatic_shapes update_bb_cache ;
  CpSpatialIndex.reindex space.spstatic_shapes

let reindex_shape space shape =
  assert (space.splocked = 0) ;
  let body = shape.shbody in
  CpShape.update shape body.bp body.brot ;

  CpSpatialIndex.reindex_object space.spactive_shapes shape (Int64.of_int shape.shhashid) ;
  CpSpatialIndex.reindex_object space.spstatic_shapes shape (Int64.of_int shape.shhashid)

let reindex_shapes_for_body space body =
  CpPrivate.Body.foreach_shape body (reindex_shape space)
    
let use_spatial_hash space dim count =
  let copy_shapes shape index = 
    CpSpatialIndex.insert index shape (Int64.of_int shape.shhashid)
  in
  let static_shapes' = CpSpatialIndex.make_space_hash dim count CpShape.get_bb None in
  let active_shapes' = CpSpatialIndex.make_space_hash dim count CpShape.get_bb (Some static_shapes') in
  
  CpSpatialIndex.each space.spstatic_shapes (fun sh -> copy_shapes sh static_shapes') ;
  CpSpatialIndex.each space.spactive_shapes (fun sh -> copy_shapes sh active_shapes') ;
  
  space.spstatic_shapes <- static_shapes' ;
  space.spactive_shapes <- active_shapes'
      

let process_components space dt =
  let sleep' = space.spsleep_time_threshold <> infinity in
  let bodies = space.spbodies in

  if sleep'
  then begin
    let dv = space.spidle_speed_threshold in
    let dvsq = (if dv <> 0. then dv *. dv else (CpVector.lengthsq space.spgravity) *. dt *. dt) in
    List.iter (fun body ->
      let ke_threshold = (if dvsq <> 0. then body.bm *. dvsq else 0.) in
      ComponentNode.(body.bnode.idle_time <- (if CpBody.kinetic_energy body > ke_threshold then 0. else body.bnode.idle_time +. dt))
    ) bodies
  end ;

  let arbiters = space.sparbiters in
  List.iter (fun arb -> CpBody.(
    let a = arb.abody_a and b = arb.abody_b in
    if sleep'
    then begin
      if (is_rogue b && not (is_static b)) || is_sleeping a then activate a ;
      if (is_rogue a && not (is_static b)) || is_sleeping b then activate b
    end ;
    push_arbiter a arb ;
    push_arbiter b arb
  ) ) arbiters ;

  if sleep'
  then begin
    let constraints = space.spconstraints in
    List.iter (fun constr -> CpBody.(
      let a = constr.ca and b = constr.cb in
      if is_rogue b && not (is_static b) then activate a ;
      if is_rogue a && not (is_static a) then activate b
    ) ) constraints ;
    
    List.iter (fun body -> CpSpaceComponent.(
      if Component.root (Some body) = None
      then begin 
        Component.flood_fill body body ;
        if not (Component.active (Some body) space.spsleep_time_threshold)
        then begin
          space.spsleeping_components <- body :: space.spsleeping_components ;
          CpPrivate.Body.foreach_component (Some body) (deactivate_body space)
        end
        else ComponentNode.(body.bnode.root <- None ; body.bnode.next <- None )
      end
    else ComponentNode.(body.bnode.root <- None ; body.bnode.next <- None )
    ) ) bodies
  end

let collide_shapes space a b =
  let query_reject a b =
    not (CpBB.intersects a.shbb b.shbb) 
    || a.shbody == b.shbody
    || (a.shgroup <> 0 && a.shgroup = b.shgroup)
    || (a.shlayers land b.shlayers) = 0
    || (a.shbody.bm = infinity  && b.shbody.bm = infinity)
  in
  if not (query_reject a b)
  then
    let handler = CpOption.(!?(lookup_handler space a.shcollision_type b.shcollision_type)) in
    let sensor = a.shsensor || b.shsensor in
    if (not sensor) || handler != default_collision_handler
    then
      let (a,b) =
        if CpShape.(get_type a > get_type b)
        then (b,a)
        else (a,b)
      in
      let contacts = CpContactBuffer.get_array space max_contacts_per_arbiter in
      let num_contacts = CpCollision.collide_shapes a b contacts in
      if num_contacts <> 0
      then begin
        CpContactBuffer.push_contacts space num_contacts ;
        let contacts = CpArray.subrange contacts 0 num_contacts in

        let shape_pair = (a,b) in
        let arb_hashid = CpPrivate.hash_pair a.shhashid b.shhashid in
        let arb = CpHashSet.insert space.spcached_arbiters arb_hashid shape_pair ArbiterSet.trans space in
        CpArbiter.update arb contacts handler a b ;

        if arb.astate = FirstColl && not (handler.chbegin arb space)
        then CpArbiter.ignore arb ;

        if (arb.astate <> Ignore) &&
          handler.chpresolve arb space &&
          not sensor
        then space.sparbiters <- arb :: space.sparbiters
        else begin
          CpContactBuffer.pop_contacts space num_contacts ;
          arb.acontacts <- CpArray.make [||] 0 0;
          if arb.astate <> Ignore then arb.astate <- Normal
        end ;
        arb.astamp <- space.spstamp
      end
        
let step space dt =
  if dt <> 0.
  then begin
    space.spstamp <- space.spstamp + 1 ;
    let prev_dt = space.spcurr_dt in
    space.spcurr_dt <- dt ;

    let bodies = space.spbodies in
    let constraints = space.spconstraints in
    let arbiters = space.sparbiters in

    List.iter (fun arb -> begin
      arb.astate <- Normal ;
      if not (CpBody.is_sleeping arb.abody_a) && not (CpBody.is_sleeping arb.abody_b)
      then CpArbiter.unthread arb
    end ) arbiters ;
    space.sparbiters <- [] ; (* ?? *)

    lock space ;
    List.iter (fun body -> body.bposition_func body dt) bodies ;
    CpContactBuffer.push_fresh space ;
    CpSpatialIndex.each space.spactive_shapes 
      (fun shape -> let body = shape.shbody in ignore (CpShape.update shape body.bp body.brot));
    CpSpatialIndex.reindex_query space.spactive_shapes (collide_shapes space) ;
    unlock space false ;

    process_components space dt ;

    lock space;
    CpHashSet.filter space.spcached_arbiters (ArbiterSet.filter space) ;
    let slop = space.spcollision_slop in
    let bias_coef = 1. -. (space.spcollision_bias ** dt) in
    List.iter (fun arb -> CpArbiter.prestep arb dt slop bias_coef) arbiters ;
    List.iter (fun con -> assert false) constraints ;

    let damping = space.spdamping ** dt in
    let gravity = space.spgravity in
    List.iter (fun body -> body.bvelocity_func body gravity damping dt) bodies ;

    let dt_coef = (if prev_dt = 0. then 0. else dt /. prev_dt) in
    List.iter (fun arb -> CpArbiter.apply_cached_impulse arb dt_coef) arbiters ;

    List.iter (fun con -> assert false) constraints ;
   
    for i=0 to (space.spiterations - 1)
    do
      List.iter (fun arb -> CpArbiter.apply_impulse arb) arbiters ;
      List.iter (fun con -> assert false) constraints
    done ;

    List.iter (fun con -> assert false) constraints ;

    List.iter (fun arb -> 
      let handler = CpOption.(!?(arb.ahandler)) in
      handler.chpostsolve arb space) arbiters ;
    unlock space true

  end

(*

  module HashedArbiter : Hashtbl.HashedType =
  struct
  type t = CpArbiter.arbiter
  let equal a b
  end

  class space =
  object
  val mutable iterations = 10
  val mutable gravity = CpVector.zero
  val mutable damping = 1.
  val mutable idle_speed_threshold = 0.
  val mutable sleep_time_threshold = infinity
  val mutable collision_slop = 0.1
  val mutable collision_bias = (1. -. 0.1) ** 60.
  val mutable collision_persistence = 3
  val mutable enable_contact_graph = false
  val staticBody = 

(* private fields *)
  val stamp = 0
  val curr_dt
  val mutable bodies = []
  val mutable roused_bodies = []
  val mutable sleeping_components = []

  val static_shapes
  val active_shapes

  val arbiters = []
  val contact_buffers_head
  val cached_arbiters
  val pooled_arbiters = []
  val constraints

  val allocated_buffers = []
  val locked = 0

  val collision_handlers = 
  val default_handler =

  val skip_post_step
  val postStepCallbacks

  method get_iterations = iterations
  method set_iterations v = iterations <- v
  method get_gravity = gravity
  method set_gravity v = gravity <- v
  method get_damping = damping
  method set_damping v = damping <- v
  method get_idle_speed_threshold = idle_speed_threshold
  method set_idle_speed_threshold v = idle_speed_threshold <- v
  method get_sleep_time_threshold = sleep_time_threshold
  method set_sleep_time_threshold v = sleep_time_threshold <- v
  method get_collision_slop = collision_slop
  method set_collision_slop v = collision_slop <- v
  method get_collision_bias = collision_bias
  method set_collision_bias v = collision_bias <- v
  method get_collision_persistence = collision_persistence
  method set_collision_persistence v = collision_persistence <- v
  method get_enable_contact_graph = enable_contact_graph
  method set_enable_contact_graph v = enable_contact_graph <- v
  method get_static_body = static_body
  method get_current_time_step = curr_dt

  method is_locked = locked <> 0

  method set_default_collision_handler ~begin' ~presolve ~postsolve ~separate = 
  self#assert_unlocked ;
  let handler = CpCollisionHandler.({
  begin' = get_if begin' always_collide ;
  presolve = get_if presolve always_collide ;
  postsolve = get_if post_solve nothing ;
  separate = get_if separate nothing
  }) in
  default_handler <- handler ;
  CpCollisionHandlerHashSet.set_default_value collisionHandler defaultHandler
  
  method add_collision_handler a b ~begin' ~presolve ~postsolve ~separate =
  


  method remove_collision_handler a b

  method add_shape shape =
  let body = shape#get_body in
  if body#is_static 
  then self#add_static_shape shape
  else begin 
  assert shape#get_space = None ;
  self#assert_unlocked ;

  body#activate ;
  body#add_shape shape ;
  shape#update body#get_pos body#get_rot ;
  CpSpatialIndex.insert active_shapes shape shape#hashid ;
  shape#set_space self ;
  shape
  end


  method add_static_shape shape =
  assert shape#get_space = None ;
  self#assert_unlocked ;
  let body = shape#get_body in
  body#add_shape shape ;
  shape#update body#get_pos body#get_rot ;
  CpSpatialIndex.insert static_shapes shape shape#hashid ;
  shape#set_space self ;
  shape

  method add_body body =
  assert (not body#is_static) ;
  assert body#get_space = None ;
  self#assert_unlocked ;
  bodies <- Array.append bodies [| body |] ;
  body#set_space self ;
  body

  method add_constraint constraint' =
  assert constraint'#get_space = None ;
  self#assert_unlocked ;

  constraint'#a#activate ;
  constraint'#b#activate ;
  constraints <- Array.append constraints [| constraint' |] ;

  let a = constraint'#a and b = constraint'#b in
  constraint'#set_next_a a#constraint_list ; a#set_constraint_list constraint' ;
  constraint'#set_next_b b#constraint_list ; b#set_constraint_list constraint' ;
  constraint'#set_space space ;
  constraint'

  method remove_shape shape =
  let body = shape#body in
  if body#is_static 
  then self#remove_static_shape shape
  else begin
  assert (self#contains_shape shape) ;
  self#assert_unlocked ;

  body#activate ;
  body#remove_shape shape ;
  space#filter_arbiters body (Some shape) ;
  CpSpatialIndex.remove space.active_shapes shape shape#hashid ;
  shape#set_space None
  end

  method remove_static_shape shape =
  assert (self#contains_shape shape) ;
  self#assert_unlocked ;
  
  let body = shape#body in
  if body#is_static then body#activate_static shape ;
  body#remove_shape shape ;
  self#filter_arbiters body (Some shape) ;
  CpSpatialIndex.remove space.static_shapes shape shape#hashid ;
  shape#set_space None

  method remove_body body =
  assert (self#contains_body body) ;
  self#assert_unlocked ;
  body#activate ;
  bodies <- List.filter (fun b -> b = body) bodies ;
  body#set_space None

  method remove_constraint constraint' =
  assert (self#contains_constraint constraint') ;
  space#assert_unlocked ;
  
  constraint'#a#activate ;
  constraint'#b#activate ;
  constraints <- List.filter (fun c -> c = constraint') constraints ;
  body#remove_constraint constraint'#a constraint' ;
  body#remove_constraint constraint'#b constraint' ;
  constraint'#set_space None
  
  method contains_shape shape =
  shape#get_space = self

  method contains_body body =
  body#get_space = self

  method contains_constraint constraint' =
  constraint'#get_space = self

  method add_post_step_callback func key =

  method point_query point layers group func =

  method point_query_first point layers group =

  method nearest_point_query point max_distance layers group func =

  method nearest_point_query_nearest point max_distance layers group =

  method segment_query start end' layers group func =

  method segment_query_first start end' layers group =

  method bb_query bb layers group func =

  method shape_query shape func =

  method activate_shapes_touching_shape shape =

  method each_body func =
  space#lock ;
  List.iter func bodies ;
  List.iter (fun lst -> List.iter func lst) sleeping_components ;
  space#unlock true

  method each_shape func =
  space#lock ;
  CpSpatialIndex.each active_shapes func ;
  CpSpatialIndex.each static_shaoes func ;
  space#unlock true 

  method each_constraint func =
  space#lock ;
  List.iter func constraints
  space#unlock true

  method reindex_static =
  let update_bb_cache shape =
  let body = shape#body in
  shape#update body#get_pos body#get_rot
  in
  assert (not locked) ;
  CpSpatialIndex.each static_shapes update_bb_cache ;
  CpSpatialIndex.reindex static_shapes

  method reindex_shape shape =
  assert (not locked) ;
  let body = shape#body in
  shape#update body#get_pos body#get_rot ;

  CpSpatialIndex.reindex_object active_shapes shape shape#hashid ;
  CpSpatialIndex.reindex_object static_shapes shape shape#hashid

  method reindex_shapes_for_body body =
  let list = body#shape_list in
  List.iter (fun shape -> self#reindex_shape shape) list ;

  method use_spatial_hash dim count =
  let copy_shapes shape index = 
  CpSpatialIndex.insert index shape shape#hashid
  in
  let get_bb = (fun shape -> shape#get_bb) in
  let static_shapes' = CpSpaceHash.make dim count get_bb None in
  let active_shapes' = CpSpaceHash.make dim count get_bb (Some static_shapes') in

  CpSpatialIndex.each static_shapes (copy_shapes static_shapes') ;
  CpSpatialIndex.each active_shapes (copy_shapes active_shapes') ;

  static_shapes <- static_shapes' ;
  active_shapes <- active_shapes'
  

  method step dt =

  method filter_arbiters body shape =
  let filter arb =
  let match_shape other = 
  match shape with 
  | Some s  -> s = other
  | None -> true
  in
  if (body = arb#body_a && (match_shape arb#a)) ||
  (body = arb#body_b && (match_shape arb#b))
  then begin
  if shape <> None && arb#state <> CpArbiter.state_cached
  then arb#call_separate space ;
  arb#unthread ;
  space#remove_arbiter arb ;
  false
  end
  else true
  in	
  space#lock ;
  CpHashSet.filter cached_arbiter filter ;
  space#unlock true
  end
*)