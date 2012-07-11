open OChipmunkDemo
open Ochipmunk

class pyramid_stack =
object
  inherit demo "Pyramid Stack"

  method init =
    let space = CpSpace.make () in
    CpSpace.set_iterations space 30 ;
    CpSpace.set_gravity    space (CpVector.make 0. 100.) ;
    CpSpace.set_sleep_time_threshold space 0.5 ;
    CpSpace.set_collision_slop space 0.5 ;

    let static_body = CpSpace.get_static_body space in

    let shape = CpShape.make_segment static_body 
      (CpVector.make (-.320.) 240.) (CpVector.make (-.320.) (-.240.)) 0. in
    ignore (CpSpace.add_shape space shape) ;
    CpShape.set_elasticity shape 1. ;
    CpShape.set_friction shape 1. ;
    CpShape.set_layers shape not_grabable_mask ;

    let shape = CpShape.make_segment static_body 
      (CpVector.make 320. 240.) (CpVector.make 320. (-.240.)) 0. in
    ignore (CpSpace.add_shape space shape) ;
    CpShape.set_elasticity shape 1. ;
    CpShape.set_friction shape 1. ;
    CpShape.set_layers shape not_grabable_mask ;

    let shape = CpShape.make_segment static_body 
      (CpVector.make (-.320.) 240.) (CpVector.make 320. 240.) 0. in
    ignore (CpSpace.add_shape space shape) ;
    CpShape.set_elasticity shape 1. ;
    CpShape.set_friction shape 1. ;
    CpShape.set_layers shape not_grabable_mask ;



    for i = 0 to 13
    do
      for j = 0 to i
      do
        let body = CpSpace.add_body space (CpBody.make 1. (Cp.moment_for_box 1. 30. 30.)) in
        CpBody.set_pos body (CpVector.make (float (j*32 - i*16)) (float (i*32 - 300))) ;
        
        let shape = CpSpace.add_shape space (CpShape.make_box body 30. 30.) in
        CpShape.set_elasticity shape 0. ;
        CpShape.set_friction shape 0.8
      done
    done ;

    let radius = 15. in
    let body = CpSpace.add_body space (CpBody.make 10. (Cp.moment_for_circle 10. 0. radius CpVector.zero)) in
    CpBody.set_pos body (CpVector.make 0. (240. -. radius -. 5.)) ;
    let shape = CpSpace.add_shape space (CpShape.make_circle body radius CpVector.zero) in
    CpShape.set_elasticity shape 0. ;
    CpShape.set_friction shape 0.9 ;
    space

  method update ticks space =
    let steps = 3 in
    let dt = 1./.60./. float steps in
    for i = 0 to steps - 1
    do CpSpace.step space dt done

  method draw target space =
    default_draw_impl target space

  method destroy = ()
end
