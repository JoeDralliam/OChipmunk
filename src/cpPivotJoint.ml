module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  module ConstraintUtils = CpConstraintUtils.Make(Param)
  module Constraint = CpConstraint.Make(Param)
  module Body = CpBody.Make(Param)
  open Type
  open ConstraintUtils

  class pivot_joint a b anchor1 anchor2 =
  object
    inherit [constraint'] ConstraintClass.t

    val anchr1 = anchor1
    val anchr2 = anchor2

    val mutable r1 = CpVector.zero
    val mutable r2 = CpVector.zero
    val mutable k1 = CpVector.zero
    val mutable k2 = CpVector.zero

    val mutable j_acc = CpVector.zero
    val mutable j_max_len = 0.
    val mutable bias = CpVector.zero

    method prestep con dt =
      let a = con.ca in
      let b = con.cb in
      r1 <- CpVector.rotate anchr1 a.brot ;
      r2 <- CpVector.rotate anchr2 b.brot ;

      let (k1', k2') = k_tensor a b r1 r2 in
      k1 <- k1' ; k2 <- k2' ;

      j_max_len <- j_max con dt ;

      let delta = CpVector.(sub (add b.bp r2) (add a.bp r1)) in
      bias <- CpVector.(clamp (mult delta (-.bias_coef con.cerror_bias dt/.dt)) con.cmax_bias) ;


    method apply_cached_impulse con dt_coef =
      let a = con.ca in
      let b = con.cb in
      let j = CpVector.mult j_acc dt_coef in
      apply_impulses a b r1 r2 j

    method apply_impulse con =
      let a = con.ca in
      let b = con.cb in

      let vr = relative_velocity a b r1 r2 in

      let j = mult_k (CpVector.sub bias vr) k1 k2 in
      let j_old = j_acc in
      j_acc <- CpVector.(clamp (add j_acc j) j_max_len) ;
      let j = CpVector.sub j_acc j_old in

      apply_impulses a b r1 r2 j

    method get_impulse _ =
      CpVector.length j_acc
  end
    
  let make2 a b anchor1 anchor2 =
    Constraint.make (new pivot_joint a b anchor1 anchor2) a b

  let make a b pivot =
    let anchr1 = Body.world2local a pivot in
    let anchr2 = Body.world2local b pivot in
    make2 a b anchr1 anchr2
end
