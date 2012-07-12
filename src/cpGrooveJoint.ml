module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  module ConstraintUtils = CpConstraintUtils.Make(Param)
  module Constraint = CpConstraint.Make(Param)
  module Body = CpBody.Make(Param)
  open Type
  open ConstraintUtils

class groove_joint a b groove_a groove_b anchor2 =
object (self)
  inherit [constraint'] ConstraintClass.t

  val grv_n = CpVector.(perp (normalize (sub groove_b groove_a)))
  val grv_a = groove_a
  val grv_b = groove_b
  val anchr2 = anchor2

  val mutable grv_tn = CpVector.zero
  val mutable clamp = 0.
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

    let ta = Body.local2world a grv_a in
    let tb = Body.local2world b grv_b in

    let n = CpVector.rotate grv_n a.brot in
    let d = CpVector.dot ta n in

    grv_tn <- n ;
    r2 <- CpVector.rotate anchr2 b.brot ;

    let td = CpVector.(cross (add b.bp r2) n) in

    if td <= CpVector.cross ta n
    then (clamp <- 1. ; r1 <- CpVector.sub ta a.bp)
    else if td >= CpVector.cross tb n
    then (clamp <- -.1. ; r1 <- CpVector.sub tb a.bp)
    else (clamp <- 0. ; r1 <- CpVector.(sub (add (mult (perp n) (-.td)) (mult n d)) a.bp)) ;


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

  method private groove_constrain j =
    let n = grv_tn in
    let j_clamp = 
      if clamp *. CpVector.cross j n > 0.
      then j
      else CpVector.project j n
    in
    CpVector.clamp j_clamp j_max_len

  method apply_impulse con =
    let a = con.ca in
    let b = con.cb in

    let vr = relative_velocity a b r1 r2 in

    let j = mult_k (CpVector.sub bias vr) k1 k2 in
    let j_old = j_acc in
    j_acc <- self#groove_constrain (CpVector.add j_old j) ;
    let j = CpVector.sub j_acc j_old in

    apply_impulses a b r1 r2 j

  method get_impulse _ =
    CpVector.length j_acc
end
 
let make a b groove_a groove_b anchor2 =
  Constraint.make (new groove_joint a b groove_a groove_b anchor2) a b
end
