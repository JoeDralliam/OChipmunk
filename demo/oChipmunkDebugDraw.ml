open OcsfmlGraphics
open Cp.Type

let la_color l alpha = Color.rgba l l l alpha


let line_color = Color.rgba 200 210 230 255
let constraint_color = Color.rgba 0 191 0 255
let shape_alpha = 255

let point_line_scale = ref 1.

let color_from_hash hash alpha =
  let hash =  Int64.(
    let hash = (add    (add    hash 0x7ed55d16L) (shift_left  hash 12)) in
    let hash = (logxor (logxor hash 0xc761c23cL) (shift_right hash 19)) in
    let hash = (add    (add    hash 0x165667b1L) (shift_left  hash  5)) in
    let hash = (logxor (add    hash 0xd3a2646cL) (shift_left  hash  9)) in
    let hash = (add    (add    hash 0xfd7046c5L) (shift_left  hash  3)) in
    let hash = (logxor (logxor hash 0xb55a4f09L) (shift_right hash 16)) in
    hash
  ) in

  let r = Int64.(to_int (logand (shift_right hash  0) 0xffL)) in
  let g = Int64.(to_int (logand (shift_right hash  8) 0xffL)) in
  let b = Int64.(to_int (logand (shift_right hash 16) 0xffL)) in

  let mx = max (max r g) b in
  let mn = min (min r g) b in
  let intensity = 0.75 in

  if mn = mx
  then Color.rgba (truncate (255. *. intensity)) 0 0 alpha
  else let coef = truncate (float alpha *. intensity) / (mx - mn) in
       Color.rgba ((r - mn) * coef) ((g - mn) * coef) ((b - mn) * coef) alpha

let color_for_shape shape =
  if Cp.Shape.get_sensor shape
  then la_color 255 0
  else begin
    let body = shape.shbody in
    if Cp.Body.is_sleeping body
    then la_color 51 255
    else if ComponentNode.(body.bnode.idle_time) > CpOption.(!?(shape.shspace).spsleep_time_threshold)
    then la_color 170 250
    else color_from_hash (Int64.of_int shape.shhashid) shape_alpha
  end 

let draw_circle (target:#render_target) center angle radius line_color fill_color =
  let circle = Cp.Vector.(new circle_shape 
                           ~position:(center.x,center.y) 
                           ~origin:(radius,radius)
                           ~rotation:angle 
                           ~radius
                           ~outline_color:line_color
                           ~fill_color ()) in
  target#draw circle

let draw_segment (target:#render_target) a b color =
  let points = Cp.Vector.([ mk_vertex ~position:(a.x,a.y) ~color () ; mk_vertex ~position:(b.x,b.y) ~color () ]) in
  let segment = new vertex_array ~primitive_type:Lines points in
  target#draw segment

let draw_fat_segment (target:#render_target) a b radius outline_color fill_color =
  if radius <> 0.
  then Cp.Vector.(
    let position = (a.x,a.y) in
    let size = (dist a b, radius) in
    let rotation = toangle (sub b a) in
    let origin = (0., radius /. 2.) in
    let rect = new rectangle_shape ~position ~size ~origin ~rotation ~fill_color ~outline_color () in
    target#draw rect
  )
  else draw_segment target a b outline_color

let draw_polygon (target:#render_target) verts outline_color fill_color =
  let lgth = Array.length verts in
  let to_sf_point v = Cp.Vector.(v.x,v.y) in
  let rec to_sf_point_list idx =
    if idx < lgth
    then (to_sf_point verts.(idx)) :: (to_sf_point_list (idx+1))
    else []
  in  
  let points = to_sf_point_list 0 in
let shp = new convex_shape ~points ~outline_color ~fill_color () in
  target#draw shp

let draw_points (target:#render_target) size verts color =
  let lgth = Array.length verts in
  let to_sf_vertex v = mk_vertex ~position:Cp.Vector.(v.x,v.y) ~color () in
  let rec to_sf_vertex_list idx =
    if idx < lgth
    then (to_sf_vertex verts.(idx)) :: (to_sf_vertex_list (idx+1))
    else []
  in
  let points = to_sf_vertex_list 0 in
  let vtx_arr = new vertex_array ~primitive_type:Points points in
  target#draw vtx_arr

let draw_bb (target:#render_target) bb color =
  let position = Cp.BB.(bb.l,bb.t) in 
  let size = Cp.BB.(bb.r -. bb.l, bb.b -. bb.t) in
  let shp = new rectangle_shape ~position ~size ~fill_color:color () in
  target#draw shp

let draw_shape target shape =
  let module Circle  = Cp.ShapeType.CircleImpl    in
  let module Segment = Cp.ShapeType.SegmentImpl   in
  let module Poly    = Cp.ShapeType.PolyShapeImpl in
  let body = shape.shbody in
  let color = color_for_shape shape in
  match shape.shstrategy with
    | Cp.ShapeType.Circle c -> 
        Circle.(draw_circle target c.tc body.ba c.r line_color color)
    | Cp.ShapeType.Segment s -> 
        Segment.(draw_fat_segment target s.ta s.tb s.r line_color color)
    | Cp.ShapeType.Poly p ->
        Poly.(draw_polygon target p.t_verts line_color color)

let draw_shapes target space =
  Cp.Space.each_shape space (draw_shape target)


let draw_collision_points (target:#render_target) space =
  let arbiters = space.sparbiters in
  let fill_color = Color.rgb 255 0 0 in
  let radius = 2. *. !point_line_scale in
  let origin = (radius, radius) in
  List.iter (fun arb ->
    CpArray.iter (fun contact ->
      let v = Cp.Contact.(contact.p) in
      let position = Cp.Vector.(v.x,v.y) in
      let point = new circle_shape ~position ~radius ~origin ~fill_color () in
      target#draw point
    ) arb.acontacts
  ) arbiters
