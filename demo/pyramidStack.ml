open OChipmunkDemo

class pyramid_stack =
object
  inherit demo "Pyramid Stack"

  method init =
    let space = Cp.Space.make () in
    Cp.Space.set_iterations space 30 ;
    Cp.Space.set_gravity    space (Cp.Vector.make 0. 100.) ;
    Cp.Space.set_sleep_time_threshold space 0.5 ;
    Cp.Space.set_collision_slop space 0.5 ;

    let static_body = Cp.Space.get_static_body space in

    let shape = Cp.Shape.make_segment static_body 
      (Cp.Vector.make (-.320.) 240.) (Cp.Vector.make (-.320.) (-.240.)) 0. in
    ignore (Cp.Space.add_shape space shape) ;
    Cp.Shape.set_elasticity shape 1. ;
    Cp.Shape.set_friction shape 1. ;
    Cp.Shape.set_layers shape not_grabable_mask ;

    let shape = Cp.Shape.make_segment static_body 
      (Cp.Vector.make 320. 240.) (Cp.Vector.make 320. (-.240.)) 0. in
    ignore (Cp.Space.add_shape space shape) ;
    Cp.Shape.set_elasticity shape 1. ;
    Cp.Shape.set_friction shape 1. ;
    Cp.Shape.set_layers shape not_grabable_mask ;

    let shape = Cp.Shape.make_segment static_body 
      (Cp.Vector.make (-.320.) 240.) (Cp.Vector.make 320. 240.) 0. in
    ignore (Cp.Space.add_shape space shape) ;
    Cp.Shape.set_elasticity shape 1. ;
    Cp.Shape.set_friction shape 1. ;
    Cp.Shape.set_layers shape not_grabable_mask ;



    for i = 0 to 13
    do
      for j = 0 to i
      do
        let body = Cp.Space.add_body space (Cp.Body.make 1. (OChipmunk.Cp.moment_for_box 1. 30. 30.)) in
        Cp.Body.set_pos body (Cp.Vector.make (float (j*32 - i*16)) (float (i*32 - 300))) ;
        
        let shape = Cp.Space.add_shape space (Cp.Shape.make_box body 30. 30.) in
        Cp.Shape.set_elasticity shape 0. ;
        Cp.Shape.set_friction shape 0.8
      done
    done ;

    let radius = 15. in
    let body = Cp.Space.add_body space (Cp.Body.make 10. (OChipmunk.Cp.moment_for_circle 10. 0. radius Cp.Vector.zero)) in
    Cp.Body.set_pos body (Cp.Vector.make 0. (240. -. radius -. 5.)) ;
    let shape = Cp.Space.add_shape space (Cp.Shape.make_circle body radius Cp.Vector.zero) in
    Cp.Shape.set_elasticity shape 0. ;
    Cp.Shape.set_friction shape 0.9 ;
    space

  method update ticks space =
    let steps = 3 in
    let dt = 1./.60./. float steps in
    for i = 0 to steps - 1
    do Cp.Space.step space dt done

  method draw target space =
    default_draw_impl target space

  method destroy = ()
end
