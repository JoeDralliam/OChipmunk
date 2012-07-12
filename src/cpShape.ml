open CpShapeType
module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  module Private = CpPrivate.Make(Param)
  module NearestPointQueryInfo = CpNearestPointQueryInfo.Make(Param)
  module SegmentQueryInfo = CpSegmentQueryInfo.Make(Param)
  module Body = CpBody.Make(Param)
  module CircleShape = CpCircleShape.Make(Param)
  module SegmentShape = CpSegmentShape.Make(Param)
  module PolyShape = CpPolyShape.Make(Param)
  module BoxShape = CpBoxShape.Make(Param)
  open Type

type t = shape'

let do_if f opt = match opt with
  | Some v -> f v
  | None -> ()

let eql = Private.Shape.eql

let id_counter = ref 0
let reset_id_counter () =
  id_counter := 0


let get_bb shape = shape.shbb

let get_sensor shape = shape.shsensor
let set_sensor shape v = 
  Body.activate shape.shbody ;
  shape.shsensor <- v

let get_elasticity shape = shape.she
let set_elasticity shape v = shape.she <- v

let get_friction shape = shape.shu
let set_friction shape v = 
  Body.activate shape.shbody ;
  shape.shu <- v

let get_surface_velocity shape = shape.shsurface_v
let set_surface_velocity shape v = 
  Body.activate shape.shbody ;
  shape.shsurface_v <- v

let get_collision_type shape = shape.shcollision_type
let set_collision_type shape v = 
  Body.activate shape.shbody ;
  shape.shcollision_type <- v

let get_group shape = shape.shgroup
let set_group shape v = 
  Body.activate shape.shbody ;
  shape.shgroup <- v

let get_layers shape = shape.shlayers
let set_layers shape v = 
  Body.activate shape.shbody ;
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

    shhashid = !id_counter ;

    shdata = None 
  } in
  incr id_counter ;
  shape

let active shape =
  (shape.shprev <> None) || CpOption.(shape.shbody.bshape_list ==? shape)
    
let make_circle body radius offset =
  make (Circle (CircleShape.make radius offset)) body

let make_segment body a b r =
  make (Segment (SegmentShape.make a b r)) body

let make_poly body verts offset =
  make (Poly (PolyShape.make verts offset)) body

let make_box body width height =
  make (Poly (BoxShape.make width height)) body

let make_box2 body bb =
  make (Poly (BoxShape.make2 bb)) body

let set_body shape body = 
  assert (not (active shape)) ;
  shape.shbody <- body

let update = Private.Shape.update


let cache_bb shape =
  let body = shape.shbody in
  update shape body.bp body.brot

let nearest_point_query shape p =
  let impl = dispatch shape
    CircleShape.nearest_point_query 
    SegmentShape.nearest_point_query 
    PolyShape.nearest_point_query
  in
  let info = impl p in
  ( NearestPointQueryInfo.(info.d), info )


let point_query shape p =
  let (d, _ ) = nearest_point_query shape p in
  d < 0.

let segment_query shape a b =
  let impl = dispatch shape
    CircleShape.segment_query 
    SegmentShape.segment_query 
    PolyShape.segment_query
  in
  let info = impl a b in
  ( SegmentQueryInfo.(info.shape <> None), info )

end
