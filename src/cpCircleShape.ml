open CpShapeType.CircleImpl
module Make = functor (Param : CpType.UserData) ->
struct
  module NearestPointQueryInfo = CpNearestPointQueryInfo.Make(Param)
  module SegmentQueryInfo = CpSegmentQueryInfo.Make(Param)

let get_offset circle _ = 
  circle.c

let get_radius circle _ = 
  circle.r

let cache_data circle _ p rot =
  circle.tc <- CpVector.(add p (rotate circle.c rot)) ;
  CpBB.make_for_circle circle.tc circle.r
    
let nearest_point_query circle shape p =
    let delta = CpVector.sub p circle.tc in
    let d = CpVector.length delta in
    NearestPointQueryInfo.({
      shape = Some shape ;
      p = CpVector.(add circle.tc (mult delta (circle.r /. d))) ;
      d = d -. circle.r
    })

let circle_segment_query shape center r a b default = 
  let open CpVector in
      let a = sub a center in
      let b = sub b center in
      let qa = (dot a a) -. 2. *. (dot a b) +. (dot b b) in
      let qb = -.2. *. (dot a a) +. 2. *. (dot a b) in
      let qc = (dot a a) -. r *. r in
      
      let det = qb *. qb -. 4. *. qa *. qc in
      if det >= 0. 
      then 
	let t = (-. qb -. (sqrt det)) /. (2. *. qa) in
	if 0. <= t && t <= 1. 
	then  SegmentQueryInfo.({ shape = Some shape ; t ; n = normalize (lerp a b t) })
	else default
      else default
        
        
let segment_query circle shape a b =
  let blank = SegmentQueryInfo.({ shape = None ; t = 0. ; n = CpVector.zero }) in
  circle_segment_query shape circle.tc circle.r a b blank
    
let make radius offset =
  { c = offset ; tc = CpVector.zero ; r = radius }
end
