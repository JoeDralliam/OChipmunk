module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  module ConstraintUtils = CpConstraintUtils.Make(Param)
  module Constraint = CpConstraint.Make(Param)
  open Type
  open ConstraintUtils

class gear_joint a b ph rat =
object
  inherit [constraint'] ConstraintClass.t

  val phase = ph
  val ratio = rat
  val ratio_inv = 1./.rat


  val mutable i_sum = 0.

  val mutable bias = 0.
  val mutable j_acc = 0.
  val mutable j_max_len = 0.

  method prestep con dt =
    let a = con.ca in
    let b = con.cb in

    i_sum <- 1./.(a.bi_inv*.ratio_inv +. ratio*.b.bi_inv) ;

    let max_bias = con.cmax_bias in
    bias <- CpFloat.clamp (-.(bias_coef con.cerror_bias dt)*.(b.ba*.ratio -. a.ba -. phase)/.dt) (-.max_bias) max_bias ;

    j_max_len <- j_max con dt


  method apply_cached_impulse con dt_coef =
    let a = con.ca in
    let b = con.cb in

    let j = j_acc*.dt_coef in
    a.bw <- a.bw -. j*.a.bi_inv*.ratio_inv ;
    b.bw <- b.bw +. j*.b.bi_inv

  method apply_impulse con =
    let a = con.ca in
    let b = con.cb in
    
    let wr = b.bw*.ratio -. a.bw in
      
    let j = (bias -. wr)*.i_sum in
    let j_old = j_acc in
    j_acc <- CpFloat.clamp (j_old +. j) (-.j_max_len) (j_max_len) ;
    let j = j_acc -. j_old in

    a.bw <- a.bw -. j*.a.bi_inv*.ratio_inv ;
    b.bw <- b.bw +. j*.b.bi_inv


  method get_impulse _ =
    abs_float j_acc
end
 
let make a b ph rat =
  Constraint.make (new gear_joint a b ph rat) a b

end
