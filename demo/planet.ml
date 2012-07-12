open OChipmunkDemo
open Cp.Type

let gravity_strength = 5.e6

let rand_pos radius =
  let rec impl () =
    let x = (Random.float 1.) *. (640. -. 2.*.radius) -. (320. -. radius) in
    let y = (Random.float 1.) *. (480. -. 2.*.radius) -. (240. -. radius) in
    let v = Cp.Vector.make x y in
    if Cp.Vector.length v < 85. then impl () else v
  in
  impl ()

class planet =
object
  inherit demo "Planet"

  val planet_body = Cp.Body.make infinity infinity

  method init =
    Cp.Body.set_ang_vel planet_body 0.2 ;

    let space = Cp.Space.make () in
    Cp.Space.set_iterations space 20 ;

    let add_box () =
      let size = 10. in
      let mass = 1. in
      let verts = Cp.Vector.(
        [| make (-.size) (-.size) ;
           make (-.size) (  size) ;
           make (  size) (  size) ;
           make (  size) (-.size) |]
      ) in
      let radius = Cp.Vector.(length (make size size)) in
      let pos = rand_pos radius in

      let body = Cp.Space.add_body space (Cp.Body.make mass (Ochipmunk.Cp.moment_for_poly mass verts Cp.Vector.zero)) in
      body.bvelocity_func <- (fun body gravity damping dt ->
        let p = Cp.Body.get_pos body in
        let sqdist = Cp.Vector.lengthsq p in
        let g = Cp.Vector.mult p (-.gravity_strength /. (sqdist *. (sqrt sqdist))) in
        Cp.Body.update_velocity body g damping dt
      ) ;
      Cp.Body.set_pos body pos ;

      let r = Cp.Vector.length pos in
      let v = sqrt (gravity_strength /. r) /. r in
      Cp.Body.set_vel body Cp.Vector.(mult (perp pos) v) ;

      Cp.Body.set_ang_vel body v ;
      Cp.Body.set_angle body (atan2 Cp.Vector.(pos.y) Cp.Vector.(pos.x)) ; 

      let shape = Cp.Space.add_shape space (Cp.Shape.make_poly body verts Cp.Vector.zero) in
      Cp.Shape.set_elasticity shape 0. ;
      Cp.Shape.set_friction shape 0.7
    in
    for i = 1 to 30 do add_box () done ;
    let shape = Cp.Space.add_shape space (Cp.Shape.make_circle planet_body 70. Cp.Vector.zero) in
    Cp.Shape.set_elasticity shape 1. ;
    Cp.Shape.set_friction shape 1. ;
    Cp.Shape.set_layers shape not_grabable_mask ;
    space

  method update ticks space =
    let steps = 3 in
    let dt = 1./.60./. float steps in
    for i = 0 to steps - 1
    do
      Cp.Space.step space dt ;
      Cp.Body.update_position planet_body dt ;
    done

  method draw target space =
    default_draw_impl target space

  method destroy = ()
end
