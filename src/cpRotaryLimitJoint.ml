module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  module ConstraintUtils = CpConstraintUtils.Make(Param)
  module Constraint = CpConstraint.Make(Param)
  open Type
  open ConstraintUtils

class rotary_limit_joint a b min' max' =
object
  inherit [constraint'] ConstraintClass.t

  val mn = min'
  val mx = max'


  val mutable i_sum = 0.

  val mutable bias = 0.
  val mutable j_acc = 0.
  val mutable j_max_len = 0.

  method prestep con dt =
    let a = con.ca in
    let b = con.cb in

    let dist = b.ba -. a.ba in
    let pdist =
      if dist > mx
      then mx -. dist
      else if dist < mn
      then mn -. dist
      else 0.
    in
    i_sum <- 1./.(1./.a.bi +. 1./.b.bi) ;

    let max_bias = con.cmax_bias in
    bias <- CpFloat.clamp (-.(bias_coef con.cerror_bias dt)*.pdist/.dt) (-.max_bias) max_bias ;

    j_max_len <- j_max con dt ;

    if bias = 0. then j_acc <- 0.

  method apply_cached_impulse con dt_coef =
    let a = con.ca in
    let b = con.cb in

    let j = j_acc*.dt_coef in
    a.bw <- a.bw -. j*.a.bi_inv ;
    b.bw <- b.bw +. j*.b.bi_inv

  method apply_impulse con =
    if bias <> 0.
    then begin
      let a = con.ca in
      let b = con.cb in
      
      let wr = a.bw -. b.bw in
      
      let j = -.(bias +. wr)*.i_sum in
      let j_old = j_acc in
      if bias < 0.
      then j_acc <- CpFloat.clamp (j_old +. j) 0. j_max_len
      else j_acc <- CpFloat.clamp (j_old +. j) (-.j_max_len) 0. ;
      let j = j_acc -. j_old in

      a.bw <- a.bw -. j*.a.bi_inv ;
      b.bw <- b.bw +. j*.b.bi_inv
    end

  method get_impulse _ =
    abs_float j_acc
end
 
let make a b min' max' =
  Constraint.make (new rotary_limit_joint a b min' max') a b

end
