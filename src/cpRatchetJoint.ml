module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  module ConstraintUtils = CpConstraintUtils.Make(Param)
  module Constraint = CpConstraint.Make(Param)
  open Type
  open ConstraintUtils

class ratchet_joint a b ph ratch =
object
  inherit [constraint'] ConstraintClass.t

  val mutable angle = b.ba -. a.ba
  val phase = ph
  val ratchet = ratch


  val mutable i_sum = 0.

  val mutable bias = 0.
  val mutable j_acc = 0.
  val mutable j_max_len = 0.

  method prestep con dt =
    let a = con.ca in
    let b = con.cb in

    let delta = b.ba -. a.ba in
    let diff = angle -. delta in

    let pdist =
      if diff *. ratchet > 0.
      then diff
      else begin
        angle <- floor ((delta -. phase)/.ratchet) *. ratchet +. phase ;
        0.
      end
    in
    i_sum <- 1./.(a.bi_inv +. b.bi_inv) ;

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
      
      j_acc <- CpFloat.clamp ((j_old +. j)*.ratchet) 0. (j_max_len*.abs_float ratchet) /. ratchet ;
      let j = j_acc -. j_old in

      a.bw <- a.bw -. j*.a.bi_inv ;
      b.bw <- b.bw +. j*.b.bi_inv
    end

  method get_impulse _ =
    abs_float j_acc
end
 
let make a b ph ratch =
  Constraint.make (new ratchet_joint a b ph ratch) a b

end
