open OChipmunkDemo
open Ochipmunk
open CpType
let gravity_strength = 5.e6

let rand_pos radius =
  let rec impl () =
    let x = (Random.float 1.) *. (640. -. 2.*.radius) -. (320. -. radius) in
    let y = (Random.float 1.) *. (480. -. 2.*.radius) -. (240. -. radius) in
    let v = CpVector.make x y in
    if CpVector.length v < 85. then impl () else v
  in
  impl ()

class planet =
object
  inherit demo "Planet"

  val planet_body = CpBody.make infinity infinity

  method init =
    CpBody.set_ang_vel planet_body 0.2 ;

    let space = CpSpace.make () in
    CpSpace.set_iterations space 20 ;

    let add_box () =
      let size = 10. in
      let mass = 1. in
      let verts = CpVector.(
        [| make (-.size) (-.size) ;
           make (-.size) (  size) ;
           make (  size) (  size) ;
           make (  size) (-.size) |]
      ) in
      let radius = CpVector.(length (make size size)) in
      let pos = rand_pos radius in

      let body = CpSpace.add_body space (CpBody.make mass (Cp.moment_for_circle mass size size CpVector.zero)) in
      (*      let body = CpSpace.add_body space (CpBody.make mass (Cp.moment_for_poly mass verts CpVector.zero)) in *)
      body.bvelocity_func <- (fun body gravity damping dt ->
        let p = CpBody.get_pos body in
        let sqdist = CpVector.lengthsq p in
        let g = CpVector.mult p (-.gravity_strength /. (sqdist *. (sqrt sqdist))) in
        CpBody.update_velocity body g damping dt
      ) ;
      CpBody.set_pos body pos ;

      let r = CpVector.length pos in
      let v = sqrt (gravity_strength /. r) /. r in
      CpBody.set_vel body CpVector.(mult (perp pos) v) ;

      CpBody.set_ang_vel body v ;
      CpBody.set_angle body (atan2 CpVector.(pos.y) CpVector.(pos.x)) ; 

      let shape = CpSpace.add_shape space (CpShape.make_circle body size CpVector.zero) in
(*      let shape = CpSpace.add_shape space (CpShape.make_poly body verts CpVector.zero) in *)
      CpShape.set_elasticity shape 0. ;
      CpShape.set_friction shape 0.7
    in
    for i = 1 to 30 do add_box () done ;
    let shape = CpSpace.add_shape space (CpShape.make_circle planet_body 70. CpVector.zero) in
    CpShape.set_elasticity shape 1. ;
    CpShape.set_friction shape 1. ;
    CpShape.set_layers shape not_grabable_mask ;
    space

  method update ticks space =
    let steps = 3 in
    let dt = 1./.60./. float steps in
    for i = 0 to steps - 1
    do
      CpSpace.step space dt ;
      CpBody.update_position planet_body dt ;
    done

  method draw target space =
    default_draw_impl target space

  method destroy = ()
end
