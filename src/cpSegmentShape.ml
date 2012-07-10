open CpShapeType.SegmentImpl

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
	then  CpSegmentQueryInfo.({ shape = Some shape ; t ; n = normalize (lerp a b t) })
	else default
      else default


let set_neighbors seg _ prev next =
  seg.a_tangent <- CpVector.sub prev seg.a ;
  seg.b_tangent <- CpVector.sub next seg.b

let get_a seg _ = seg.a

let get_b seg _ = seg.b

let get_normal seg _ = seg.n

let get_radius seg _ = seg.r

let cache_data seg _ p rot =
  let open CpVector in
      seg.ta <- add p (rotate seg.a rot) ;
      seg.tb <- add p (rotate seg.b rot) ;
      seg.tn <- rotate seg.n rot ;
      let (l,r) =
	if seg.ta.x < seg.tb.x
	then (seg.ta.x, seg.tb.x)
	else (seg.tb.x, seg.ta.x)
      in
      let (b,t) =
	if seg.ta.y < seg.tb.y
	then (seg.ta.y, seg.tb.y)
	else (seg.tb.y, seg.ta.y)
      in
      CpBB.make (l -. seg.r) (b -. seg.r) (r +. seg.r) (t +. seg.r)

let nearest_point_query seg shape p =
  let closest = CpVector.closest_point_on_segment p seg.ta seg.tb in

  let delta = CpVector.sub p closest in
  let d = CpVector.length delta in
  CpNearestPointQueryInfo.({
    shape = Some shape ;
    p = if d <> 0. 
      then CpVector.(add closest (mult delta (seg.r /. d))) 
      else closest ;
    d = d -. seg.r
  })
    
let segment_query seg shape a b =
  let blank = CpSegmentQueryInfo.({ shape = None ; t = 0. ; n = CpVector.zero }) in

  let n = seg.tn in
  let d = CpVector.(dot (sub seg.ta a) n) in
  let r = seg.r in
  
  let flipped_n = if d > 0. then CpVector.neg n else n in
  let seg_offset = CpVector.(sub (mult flipped_n r) a) in

  let seg_a = CpVector.add seg.ta seg_offset in
  let seg_b = CpVector.add seg.tb seg_offset in
  let delta = CpVector.sub b a in

  if CpVector.( (cross delta seg_a) *. (cross delta seg_b) ) <= 0.
  then begin
    let d_offset = d +. (if d > 0. then -.r else r) in
    let ad = -.d_offset in
    let bd = (CpVector.dot delta n) -. d_offset in

    if ad *. bd < 0.
    then 
      CpSegmentQueryInfo.({
	shape = Some shape ;
	t = ad /. (ad -. bd) ;
	n = flipped_n
      })
    else if (r <> 0.)
    then
      let (info1, info2) =
        let def = CpSegmentQueryInfo.({ shape = None ; t = 1. ; n = CpVector.zero }) in
	( circle_segment_query shape seg.ta seg.r a b def,
	  circle_segment_query shape seg.tb seg.r a b def)
      in
      if CpSegmentQueryInfo.( info1.t < info2.t )
      then info1
      else info2
    else blank
  end
  else blank

let make a b r =
  {
    a ; b ; n = CpVector.(perp (normalize (sub b a))) ;
    ta = CpVector.zero ; tb = CpVector.zero ; tn = CpVector.zero ;
    r ;
    a_tangent = CpVector.zero ; b_tangent = CpVector.zero
  }
