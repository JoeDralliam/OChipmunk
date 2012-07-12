module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  module ConstraintUtils = CpConstraintUtils.Make(Param)
  module Constraint = CpConstraint.Make(Param)
  open Type
  open ConstraintUtils

class slide_joint a b anchor1 anchor2 min' max' =
object
  inherit [constraint'] ConstraintClass.t

  val anchr1 = anchor1
  val anchr2 = anchor2
  val mn = min'
  val mx = max'

  val mutable r1 = CpVector.zero
  val mutable r2 = CpVector.zero
  val mutable n = CpVector.zero
  val mutable n_mass = 0.

  val mutable jn_acc = 0.
  val mutable jn_max = 0.
  val mutable bias = 0.

  method prestep con dt =
    let a = con.ca in
    let b = con.cb in
    r1 <- CpVector.rotate anchr1 a.brot ;
    r2 <- CpVector.rotate anchr2 b.brot ;

    let delta = CpVector.(sub (add b.bp r2) (add a.bp r1)) in
    let dist = CpVector.length delta in
    n <- CpVector.mult delta (1. /. (if dist <> 0. then dist else infinity)) ;
    let pdist =
      if dist > mx
      then (n <- CpVector.normalize_safe delta ; dist -. mx)
      else if dist < mn
      then (n <- CpVector.(neg (normalize_safe delta)) ; mn -. dist)
      else (n <- CpVector.zero ; jn_acc <- 0. ; 0.)
    in
    
    n_mass <-  1. /. (k_scalar a b r1 r2 n) ;

    let max_bias = con.cmax_bias in
    bias <- CpFloat.clamp ((-.bias_coef con.cerror_bias dt)*.pdist/.dt) (-.max_bias) max_bias ;
    jn_max <- j_max con dt

  method apply_cached_impulse con dt_coef =
    let a = con.ca in
    let b = con.cb in
    let j = CpVector.mult n (jn_acc *. dt_coef) in
    apply_impulses a b r1 r2 j

  method apply_impulse con =
    let a = con.ca in
    let b = con.cb in

    let vr = relative_velocity a b r1 r2 in
    let vrn = CpVector.dot vr n in

    let jn = (bias -. vrn) *. n_mass in
    let jn_old = jn_acc in
    jn_acc <- CpFloat.clamp (jn_old +. jn) (-.jn_max) 0. ;
    let jn = jn_acc -. jn_old in

    apply_impulses a b r1 r2 (CpVector.mult n jn)

  method get_impulse _ =
    abs_float jn_acc
end
 
let make a b anchor1 anchor2 min' max' =
  Constraint.make (new slide_joint a b anchor1 anchor2 min' max') a b
end
