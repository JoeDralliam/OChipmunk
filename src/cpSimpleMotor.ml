module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  module ConstraintUtils = CpConstraintUtils.Make(Param)
  module Constraint = CpConstraint.Make(Param)
  open Type
  open ConstraintUtils

  class simple_motor a b rat =
  object
    inherit [constraint'] ConstraintClass.t

    val rate = rat

    val mutable i_sum = 0.

    val mutable j_acc = 0.
    val mutable j_max_len = 0.

    method prestep con dt =
      let a = con.ca in
      let b = con.cb in

      i_sum <- 1./.(a.bi_inv +. b.bi_inv) ;

      j_max_len <- j_max con dt


    method apply_cached_impulse con dt_coef =
      let a = con.ca in
      let b = con.cb in

      let j = j_acc*.dt_coef in
      a.bw <- a.bw -. j*.a.bi_inv ;
      b.bw <- b.bw +. j*.b.bi_inv

    method apply_impulse con =
      let a = con.ca in
      let b = con.cb in
      
      let wr = b.bw -. a.bw +. rate in
      
      let j = (-.wr)*.i_sum in
      let j_old = j_acc in
      j_acc <- CpFloat.clamp (j_old +. j) (-.j_max_len) (j_max_len) ;
      let j = j_acc -. j_old in

      a.bw <- a.bw -. j*.a.bi_inv ;
      b.bw <- b.bw +. j*.b.bi_inv


    method get_impulse _ =
      abs_float j_acc
  end
    
  let make a b rat =
    Constraint.make (new simple_motor a b rat) a b

end
