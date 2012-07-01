type t = 
    {
      l:float;
      b:float;
      r:float;
      t:float
    }

let make l' b' r' t' =
  { l = l' ; b = b' ; r = r' ; t = t' }

let make_for_circle p r =
  make CpVector.(p.x -. r) CpVector.(p.y -. r) CpVector.(p.x +. r) CpVector.(p.y +. r)

let intersects a b =
  a.l <= b.r && b.l <= a.r && a.b <= b.t && b.b <= a.t

let contains_bb bb other =
  bb.l <= other.l && bb.r >= other.r && bb.b <= other.b && bb.t >= other.t

let contains_vect bb v =
  CpVector.(bb.l <= v.x && bb.r >= v.x && bb.b <= v.y && bb.t >= v.y)

let merge a b =
  make (min a.l b.l) (min a.b b.b) (max a.r b.r) (max a.t b.t)

let expand bb v =
  make CpVector.(min bb.l v.x) CpVector.(min bb.b v.y) CpVector.(max bb.r v.x) CpVector.(max bb.t v.y)

let area bb =
  (bb.r -. bb.l) *. (bb.t -. bb.b)

let merged_area a b =
  ( (max a.r b.r) -. (min a.l b.l) ) *. ( (max a.t b.t) -. (min a.b b.b) )

let segment_query bb a b =
  let idx = 1. /. CpVector.(b.x -. a.x) in
  let tx1 = if bb.l = CpVector.(a.x) then neg_infinity else CpVector.(bb.l -. a.x) *. idx in
  let tx2 = if bb.r = CpVector.(a.x) then infinity else CpVector.(bb.r -. a.x) *. idx in
  let txmin = min tx1 tx2 in
  let txmax = max tx1 tx2 in

  let idy = 1. /. CpVector.(b.x -. a.x) in
  let ty1 = if bb.b == CpVector.(a.y) then neg_infinity else CpVector.(bb.b -. a.y) *. idy in
  let ty2 = if bb.t == CpVector.(a.y) then infinity else CpVector.(bb.t -. a.y) *. idy in
  let tymin = min ty1 ty2 in
  let tymax = max ty1 ty2 in

  if tymin <= txmax && txmin <= tymax
  then let min_ = max txmin tymin in
       let max_ = min txmax tymax in
       if 0. <= max_ && min_ <= 1.
       then max min_ 0.
       else infinity
  else infinity

let intersects_segment bb a b =
  (segment_query bb a b) <> infinity

let clamp_vect bb v =
  let x = min (max bb.l CpVector.(v.x)) bb.r in
  let y = min (max bb.b CpVector.(v.y)) bb.t in
  CpVector.make x y

let wrap_vect bb v=
  let ix = abs_float (bb.r -. bb.l) in
  let modx = mod_float (CpVector.(v.x) -. bb.l) ix in
  let x = if modx > 0. then modx else modx +. ix in

  let iy = abs_float (bb.t -. bb.b) in
  let mody = mod_float (CpVector.(v.y) -. bb.b) iy in
  let y = if mody > 0. then mody else mody +. iy in
  
  CpVector.make (x +. bb.l) (y +. bb.b)

let proximity a b =
  (abs_float (a.l +. a.r -. b.l -. b.r)) +. (abs_float (a.b +. a.t -. b.b -. b.t))
