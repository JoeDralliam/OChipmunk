type t = { x:float ; y:float }

let zero = { x = 0. ; y = 0. }

let make x y =
  { x ; y }

let add v1 v2 =
  make (v1.x +. v2.x) (v1.y +. v2.y)

let mult v s =
  make (v.x*.s) (v.y*.s)

let dot v1 v2 =
  v1.x *. v2.x +. v1.y *. v2.y

let cross v1 v2 =
  v1.x *. v2.y -. v1.y *. v2.x

let length v =
  sqrt (dot v v)

let slerp v1 v2 t =
  let omega = acos (dot v1 v2) in
  if omega <> 0. 
  then let denom = 1. /. (sin omega) in
       add (mult v1 ((sin ((1. -. t) *. omega)) *. denom))
	 (mult v2 ((sin (t *. omega)) *. denom))
  else v1

let slerpconst v1 v2 a =
  let angle = acos (dot v1 v2) in
  slerp v1 v2 ((min a angle) /. angle)

let forangle a =
  make (cos a) (sin a)

let toangle v =
  atan2 v.y v.x

let str v =
  Printf.sprintf "(%.3f, %.3f)" v.x v.y

let sub v1 v2 =
  make (v1.x -. v2.x) (v1.y -. v2.y)

let neg v =
  make (-.v.x) (-.v.y)

let  perp v =
  make (-.v.y) v.x

let rperp v =
  make v.y (-.v.x)

let project v1 v2 =
  mult v2 ((dot v1 v2) /. (dot v2 v2))

let rotate v1 v2 =
  make (v1.x *. v2.x -. v1.y *. v2.y) (v1.x *. v2.y +. v1.y *. v2.x)

let unrotate v1 v2 =
  make (v1.x *. v2.x +. v1.y *. v2.y) (v1.y *. v2.x -. v1.x *. v2.y)

let lengthsq v =
  dot v v

let lerp v1 v2 t =
  add (mult v1 (1. -. t)) (mult v2 t)

let normalize v =
  mult v (1./.(length v))

let normalize_safe v =
  if (v.x = 0. && v.y = 0.) then zero else normalize v

let clamp v len =
  if (dot v v) > len *. len then mult (normalize v) len else v

let lerpconst v1 v2 d =
  add v1 (clamp (sub v2 v1) d)

let dist v1 v2 =
  length (sub v2 v1)

let distsq v1 v2 =
  lengthsq (sub v2 v1)

let near v1 v2 dist =
  distsq v1 v2 < dist *. dist

let closest_point_on_segment p a b =
  let delta = sub a b in
  let t = CpFloat.clamp01 ((dot delta (sub p b)) /. (lengthsq delta)) in
  add b (mult delta t)

let assert_nan v message =
  if not (v.x = v.x && v.y = v.y) 
  then Printf.eprintf message

let assert_infinite v message =
  if not ((abs_float v.x) <> infinity && (abs_float v.y) <> infinity)
  then Printf.eprintf message

let assert_sane v message =
  assert_nan v message ; assert_infinite v message
