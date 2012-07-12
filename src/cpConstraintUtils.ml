module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  module Private = CpPrivate.Make(Param)
  open Type


let j_max con dt =
  con.cmax_force *. dt

let relative_velocity a b r1 r2 =
  let v1_sum = CpVector.(add a.bv (mult (perp r1) a.bw)) in
  let v2_sum = CpVector.(add b.bv (mult (perp r2) b.bw)) in
  CpVector.sub v2_sum v1_sum

let normal_relative_velocity a b r1 r2 n =
  CpVector.dot (relative_velocity a b r1 r2) n

let apply_impulse body j r =
  body.bv <- CpVector.(add body.bv (mult j body.bm_inv) ) ;
  body.bw <- body.bw +. body.bi_inv *. CpVector.(cross r j)

let apply_impulses a b r1 r2 j =
  apply_impulse a (CpVector.neg j) r1 ;
  apply_impulse b j r2

let apply_bias_impulse body j r =
  body.bv_bias <- CpVector.(add body.bv_bias (mult j body.bm_inv)) ;
  body.bw_bias <- body.bw_bias +. body.bi_inv *. (CpVector.cross r j)

let apply_bias_impulses a b r1 r2 j =
  apply_bias_impulse a (CpVector.neg j) r1 ;
  apply_bias_impulse b j r2

let k_scalar_body body r n =
  let rcn = CpVector.cross r n in
  body.bm_inv +. body.bi_inv *. rcn *. rcn

let k_scalar a b r1 r2 n =
  let res = (k_scalar_body a r1 n) +. (k_scalar_body b r2 n) in
  Private.assert_soft (res <> 0.) "Unsolvable collision or constraint." ;
  res

let k_tensor a b r1 r2 =
  let open CpVector in
      let m_sum = a.bm_inv +. b.bm_inv in
      let k11 = m_sum and k12 = 0. in
      let k21 = 0. and k22 = m_sum in
      
      let a_i_inv = a.bi_inv in
      let r1xsq =    r1.x *. r1.x *. a_i_inv in
      let r1ysq =    r1.y *. r1.y *. a_i_inv in
      let r1nxy = -. r1.x *. r1.y *. a_i_inv in
      let k11 = k11 +. r1ysq and k12 = k12 +. r1nxy in
      let k21 = k21 +. r1nxy and k22 = k22 +. r1xsq in
        

      let b_i_inv = b.bi_inv in
      let r2xsq =    r2.x *. r2.x *. b_i_inv in
      let r2ysq =    r2.y *. r2.y *. b_i_inv in
      let r2nxy = -. r2.x *. r2.y *. b_i_inv in
      let k11 = k11 +. r2ysq and k12 = k12 +. r2nxy in
      let k21 = k21 +. r2nxy and k22 = k22 +. r2xsq in
      
      let determinant =  k11 *. k22 -. k12 *. k21 in

      let det_inv = 1. /. determinant in
      let k1 = make (  k22 *. det_inv) (-.k12 *. det_inv) in
      let k2 = make (-.k21 *. det_inv) (  k11 *. det_inv) in
      (k1, k2)

let mult_k vr k1 k2 =
  CpVector.(make (dot vr k1) (dot vr k2))

let bias_coef error_bias dt =
  1. -. (error_bias ** dt)

end
