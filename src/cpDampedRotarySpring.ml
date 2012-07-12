module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  module Private = CpPrivate.Make(Param)
  module ConstraintUtils = CpConstraintUtils.Make(Param)
  module Constraint = CpConstraint.Make(Param)
  open Type
  open ConstraintUtils

class damped_rotary_spring a b ~torque_force rest_agl stiffness dmping =
object (self)
  inherit [constraint'] ConstraintClass.t



  val rest_angle = rest_agl
  val stiffness = stiffness
  val damping = dmping
  val torque_force_func = 
    match torque_force with
      | Some func -> func
      | None -> (fun spr -> spr#default_spring_torque)

  val mutable target_wrn = 0.
  val mutable w_coef = 0.

  val mutable i_sum = 0.

  method rest_angle = rest_angle
  method stiffness = stiffness
  method damping = damping

  method default_spring_torque _ relative_angle =
    (relative_angle -. rest_angle) *. stiffness


  method prestep con dt =
    let a = con.ca in
    let b = con.cb in

    let moment = a.bi_inv +. b.bi_inv in
    Private.assert_soft (moment <> 0.) "Unsolvable spring" ;
    i_sum <- 1./.moment ;

    w_coef <- 1. -. exp (-.damping*.dt*.moment) ;
    target_wrn <- 0. ;

    let j_spring = torque_force_func self con (a.ba -. b.ba) *. dt in
    a.bw <- a.bw -. j_spring*.a.bi_inv ;
    b.bw <- b.bw +. j_spring*.b.bi_inv

  method apply_cached_impulse con dt_coef = ()

  method apply_impulse con =
    let a = con.ca in
    let b = con.cb in

    let wrn = a.bw -. b.bw in

    let w_damp = (target_wrn -. wrn)*.w_coef in
    target_wrn <- wrn +. w_damp ;
      
    let j_damp = w_damp *. i_sum in
    a.bw <- a.bw +. j_damp*.a.bi_inv ;
    b.bw <- b.bw -. j_damp*.b.bi_inv

  method get_impulse _ =
    0.
end
 
let make a b ~torque_force rest_agl stiffness dmping =
  Constraint.make ((new damped_rotary_spring a b ~torque_force rest_agl stiffness dmping) :> constraint_class') a b

end
