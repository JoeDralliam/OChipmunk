module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  module Private = CpPrivate.Make(Param)
  module ConstraintUtils = CpConstraintUtils.Make(Param)
  module Constraint = CpConstraint.Make(Param)
  open Type
  open ConstraintUtils


class damped_spring a b ~spring_force anchor1 anchor2 rest_lgth stiffness dmping =
object (self)
  inherit [constraint'] ConstraintClass.t


  val anchr1 = anchor1
  val anchr2 = anchor2

  val rest_length = rest_lgth
  val stiffness = stiffness
  val damping = dmping
  val spring_force_func = 
    match spring_force with
      | Some func -> func
      | None -> (fun spr -> spr#default_spring_force)

  val mutable target_vrn = 0.
  val mutable v_coef = 0.

  val mutable r1 = CpVector.zero
  val mutable r2 = CpVector.zero
  val mutable n_mass = 0.
  val mutable n = CpVector.zero

  method rest_length = rest_length
  method stiffness = stiffness
  method damping = damping

  method default_spring_force _ dist =
    (rest_length -. dist) *. stiffness


  method prestep con dt =
    let a = con.ca in
    let b = con.cb in
    r1 <- CpVector.rotate anchr1 a.brot ;
    r2 <- CpVector.rotate anchr2 b.brot ;

    let delta = CpVector.(sub (add b.bp r2) (add a.bp r1)) in
    let dist = CpVector.length delta in
    n <- CpVector.mult delta (1./.(if dist <> 0. then dist else infinity)) ;

    let k = k_scalar a b r1 r2 n in
    Private.assert_soft (k <> 0.) "Unsolvable spring" ;
    n_mass <- 1./.k ;

    target_vrn <- 0. ;
    v_coef <- 1. -. exp (-.damping*.dt*.k) ;

    let f_spring = spring_force_func self con dist in
    apply_impulses a b r1 r2 (CpVector.mult n (f_spring*.dt))


  method apply_cached_impulse con dt_coef = ()

  method apply_impulse con =
    let a = con.ca in
    let b = con.cb in

    let vrn = normal_relative_velocity a b r1 r2 n in

    let v_damp = (target_vrn -. vrn)*.v_coef in
    target_vrn <- vrn +. v_damp ;
      
    apply_impulses a b r1 r2 (CpVector.mult n (v_damp*.n_mass))

  method get_impulse _ =
    0.
end
 
let make a b ~spring_force anchor1 anchor2 rest_lgth stiffness dmping =
  Constraint.make ((new damped_spring a b ~spring_force anchor1 anchor2 rest_lgth stiffness dmping) :> constraint_class') a b

end
