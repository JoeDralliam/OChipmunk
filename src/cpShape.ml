open CpType
open CpShapeType
type t = shape'

let do_if f opt = match opt with
  | Some v -> f v
  | None -> ()

let eql = CpPrivate.Shape.eql

let id_counter = ref 0
let reset_id_counter () =
  id_counter := 0


let get_bb shape = shape.shbb

let get_sensor shape = shape.shsensor
let set_sensor shape v = 
  CpBody.activate shape.shbody ;
  shape.shsensor <- v

let get_elasticity shape = shape.she
let set_elasticity shape v = shape.she <- v

let get_friction shape = shape.shu
let set_friction shape v = 
  CpBody.activate shape.shbody ;
  shape.shu <- v

let get_surface_velocity shape = shape.shsurface_v
let set_surface_velocity shape v = 
  CpBody.activate shape.shbody ;
  shape.shsurface_v <- v

let get_collision_type shape = shape.shcollision_type
let set_collision_type shape v = 
  CpBody.activate shape.shbody ;
  shape.shcollision_type <- v

let get_group shape = shape.shgroup
let set_group shape v = 
  CpBody.activate shape.shbody ;
  shape.shgroup <- v

let get_layers shape = shape.shlayers
let set_layers shape v = 
  CpBody.activate shape.shbody ;
  shape.shlayers <- v

let get_body shape = shape.shbody

let no_group = 0
let all_layers = lnot 0


let dispatch shape cfunc sfunc pfunc =
  match shape.shstrategy with
    | Circle c -> cfunc c shape
    | Segment s -> sfunc s shape
    | Poly p -> pfunc p shape

let get_type shape =
  match shape.shstrategy with
    | Circle _ -> 0
    | Segment _ -> 1
    | Poly _ -> 2

let make strategy body =
  let shape = {
    shstrategy = strategy ;
    shbody = body ;
    shbb = CpBB.make 0. 0. 0. 0. ;
    shsensor = false ;

    she = 0. ;
    shu = 0. ;
    shsurface_v = CpVector.zero ;

    shcollision_type = 0 ;
    shgroup = no_group ;
    shlayers = all_layers ;

    shspace = None ;

    shnext = None ;
    shprev = None ;

    shhashid = !id_counter
  } in
  incr id_counter ;
  shape

let active shape =
  (shape.shprev <> None) || CpOption.(shape.shbody.bshape_list ==? shape)
    
let make_circle body radius offset =
  make (Circle (CpCircleShape.make radius offset)) body

let make_segment body a b r =
  make (Segment (CpSegmentShape.make a b r)) body

let make_poly body verts offset =
  make (Poly (CpPolyShape.make verts offset)) body

let make_box body width height =
  make (Poly (CpBoxShape.make width height)) body

let make_box2 body bb =
  make (Poly (CpBoxShape.make2 bb)) body

let set_body shape body = 
  assert (not (active shape)) ;
  shape.shbody <- body

let update = CpPrivate.Shape.update


let cache_bb shape =
  let body = shape.shbody in
  update shape body.bp body.brot

let nearest_point_query shape p =
  let impl = dispatch shape
    CpCircleShape.nearest_point_query CpSegmentShape.nearest_point_query CpPolyShape.nearest_point_query
  in
  let info = impl p in
  ( CpNearestPointQueryInfo.(info.d), info )


let point_query shape p =
  let (d, _ ) = nearest_point_query shape p in
  d < 0.

let segment_query shape a b =
  let impl = dispatch shape
    CpCircleShape.segment_query CpSegmentShape.segment_query CpPolyShape.segment_query
  in
  let info = impl a b in
  ( CpSegmentQueryInfo.(info.shape <> None), info )

(*
class shape body =
object (self)
  val mutable body = body
  val mutable bb = CpBB.make 0. 0. 0. 0.
  val mutable sensor = 0
  val mutable e = 0.
  val mutable u = 0.
  val mutable surface_v = CpVector.zero

  val mutable collision_type = 0
  val mutable group = no_group
  val mutable layers = all_layers

  val mutable space = None

  val mutable next = None
  val mutable prev = None

  val hashid = id_counter
  initializer incr id_counter

  method cache_bb =
    shape#update body#get_pos body#get_rot

  method update pos rot =
    bb <- self#cache_data pos rot ;
    bb

  method point_query p =
    let info = self#nearest_point_query p in
    (CpNearestPointQueryInfo.(info.d) < 0., info)

  method nearest_point_query p =
    let info = self#nearest_point_query_impl p in
    (CpNearestPointQueryInfo.(info.d), info)

  method segment_query a b =
    let info = self#segment_query_impl a b in
    (CpSegmentQueryInfo.(info.shape) <> None, info)

  method get_space = space

  method get_body = body
  method set_body v =
    assert (not shape#active) ;
    body <- v
      
  method get_bb = bb
    
  method get_sensor = sensor
  method set_sensor v =
    do_if (fun b -> b#activate) body ;
    sensor <- v
      
  method get_elasticity = e
  method set_elasticity v =
    elasticity <- v

  method get_friction = friction
  method set_friction v =
    do_if (fun b -> b#activate) body ;
    friction <- v

  method get_surface_velocity = surface_v
  method set_surface_velocity v =
    do_if (fun b -> b#activate) body ;
    surface_v <- v

  method get_collision_type = collision_type
  method set_collision_type v =
    do_if (fun b -> b#activate) body ;
    collision_type <- v

  method get_group = group
  method set_group v =
    do_if (fun b -> b#activate) body ;
    group <- v

  method get_layers = layers
  method set_layers v =
    do_if (fun b -> b#activate) body ;
    layers <- v

  (* PRIVATE *)
  method active = (prev <> None) || ( (body <> None) && (body#get_shape_list = shape))

  method private virtual cache_data: CpVector.t -> float -> CpBB.t 
  method private virtual nearest_point_query_impl: CpVector.t -> CpNearestPointQueryInfo.t
  method private virtual segment_query_impl: CpVector.t -> CpVector.t -> CpNearestPointQueryInfo.t
end



let collides a b arr =


class circle_shape radius offset =
object
  inherit shape

  val c = offset
  val mutable tc = CpVector.zero
  val r = radius

  method get_offset = c

  method get_radius = r

  method private cache_data p rot =
    tc <- Vector.(add (rotate c rot)) ;
    CpBB.make_for_circle tc r0
    
  method private nearest_point_query_impl p =
    let delta = CpVector.sub p tc in
    let d = CpVector.length delta in
    {
      shape = Some (self :> shape) ;
      p = CpVector.(add tc (mult delta (r /. d))) ;
      d = d -. r
    }

  method private segment_query_impl a b =
    circleSegmentQuery (circle :> shape) tc r a b
end

class segment_shape a b radius =
object
  inherit shape

  val a = a
  val b = b
  val n = CpVector.(perp (normalize (sub b a)))

  val mutable ta = CpVector.zero
  val mutable tb = CpVector.zero
  val mutable tn = CpVector.zero

  val r = radius

  val mutable a_tangent = CpVector.zero
  val mutable b_tangent = CpVector.zero

  method set_neighbors prev next =
    a_tangent <- CpVector.sub prev a ;
    b_tangent <- CpVector.sub next b

  method get_a = a

  method get_b = b

  method get_normal = n

  method get_radius = r

  method private cache_data p rot =
    let open CpVector in
	ta <- add p (rotate a rot) ;
	tb <- add p (rotate b rot) ;
	tn <- rotate n rot ;
	let (lef,rig) =
	  if ta.x < tb.x
	  then (ta.x, tb.x)
	  else (tb.x, ta.x)
	in
	let (bot,top) =
	  if ta.y < tb.y
	  then (ta.y, tb.y)
	  else (tb.y, ta.y)
	in
	CpBB.make (lef -. r) (bot -. r) (rig +. r) (top +. r)

  method private nearest_point_query_impl p =
    let closest = CpVector.closest_point_on_segment p ta tb in

    let delta = CpVector.sub p closest in
    let d = CpVector.length delta in
    CpNearestPointQueryInfo.({
      shape = Some (self :> shape) ;
      p = (if d <> 0.
	then CpVector.(add closest (mult delta (r /. d))) 
	else closest) ;
      d = d -. r
    }

  method private segment_query_impl a b =
    let blank = CpSegmentQueryInfo.{ shape = None ; t = 0. ; n = CpVector.zero } in
    let d = (dot (sub ta a) tn) in
    
    let flipped_n = if d > 0. then CpVector.neg tn else tn in
    let seg_offset = CpVector.(sub (mult flipped_n r) a) in

    let seg_a = CpVector.add ta seg_offset in
    let seg_b = CpVector.add tb seg_offset in
    let delta = CpVector.sub b a in

    if CpVector.( (cross delta seg_a) *. (cross delta seg_b) ) <= 0.
    then begin
      let d_offset = d +. (if d > 0. then -.r else r) in
      let ad = -.d_offset in
      let bd = (CpVector.dot delta tn) -. d_offset in

      if ab *. ad < 0.
      then 
	CpSegmentQueryInfo({
	  shape = Some (self :> shape) ;
	  t = ad /. (ad - . bd) ;
	  n = flipped_n
	})
      else if (r <> 0.)
      then
	let (info1, info2) =
	  ( circle_segment_query (self :> shape) ta r a b ,
	    circle_segment_query (self :> shape) tb r a b )
	in
	if CpSegmentQueryInfo.( info1.t < info2.t )
	then info1
	else info2
      else blank
    end
    else blank
end


*)
