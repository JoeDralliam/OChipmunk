open OChipmunkDemo
open Ochipmunk

class tumble =
object
  inherit demo "Tumble"

  val rogue_box_body = CpBody.make infinity infinity

  method init =
    let space = CpSpace.make () in
    CpSpace.set_gravity space (CpVector.make 0. 600.) ;
    CpBody.set_ang_vel rogue_box_body 0.4 ;

    let a = CpVector.make (-.200.) 200. in
    let b = CpVector.make (-.200.) (-.200.) in
    let c = CpVector.make 200. (-.200.) in
    let d = CpVector.make 200. 200. in


    let shape = CpSpace.add_shape space (CpShape.make_segment rogue_box_body a b 0.) in
    CpShape.set_elasticity shape 1. ;
    CpShape.set_friction shape 1. ;
    CpShape.set_layers shape not_grabable_mask ;

    let shape = CpSpace.add_shape space (CpShape.make_segment rogue_box_body b c 0.) in
    CpShape.set_elasticity shape 1. ;
    CpShape.set_friction shape 1. ;
    CpShape.set_layers shape not_grabable_mask ;

    let shape = CpSpace.add_shape space (CpShape.make_segment rogue_box_body c d 0.) in
    CpShape.set_elasticity shape 1. ;
    CpShape.set_friction shape 1. ;
    CpShape.set_layers shape not_grabable_mask ;

    let shape = CpSpace.add_shape space (CpShape.make_segment rogue_box_body d a 0.) in
    CpShape.set_elasticity shape 1. ;
    CpShape.set_friction shape 1. ;
    CpShape.set_layers shape not_grabable_mask ;

    let mass = 1. in
    let width = 60. in
    let height = 60. in

    for i=0 to 2
    do
      for j=0 to 6
      do
        let body = CpSpace.add_body space (CpBody.make mass (Cp.moment_for_box mass width height)) in
        CpBody.set_pos body (CpVector.make (float (i*60 - 150)) (-.float (j*30 - 150))) ;
        let shape = CpSpace.add_shape space (CpShape.make_box body width height) in
        CpShape.set_elasticity shape 0. ;
        CpShape.set_friction shape 0.7 ;        
      done
    done ;
    space

  method update ticks space =
    let steps = 3 in
    let dt = 1./.60./. float steps in
    for i = 0 to steps - 1
    do
      CpBody.update_position rogue_box_body dt ;
      CpSpace.step space dt 
    done

  method draw target space =
    default_draw_impl target space

  method destroy = ()
end
