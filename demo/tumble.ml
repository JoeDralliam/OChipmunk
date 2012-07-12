open OChipmunkDemo


class tumble =
object
  inherit demo "Tumble"

  val rogue_box_body = Cp.Body.make infinity infinity

  method init =
    let space = Cp.Space.make () in
    Cp.Space.set_gravity space (Cp.Vector.make 0. 600.) ;
    Cp.Body.set_ang_vel rogue_box_body 0.4 ;

    let a = Cp.Vector.make (-.200.) 200. in
    let b = Cp.Vector.make (-.200.) (-.200.) in
    let c = Cp.Vector.make 200. (-.200.) in
    let d = Cp.Vector.make 200. 200. in


    let shape = Cp.Space.add_shape space (Cp.Shape.make_segment rogue_box_body a b 0.) in
    Cp.Shape.set_elasticity shape 1. ;
    Cp.Shape.set_friction shape 1. ;
    Cp.Shape.set_layers shape not_grabable_mask ;

    let shape = Cp.Space.add_shape space (Cp.Shape.make_segment rogue_box_body b c 0.) in
    Cp.Shape.set_elasticity shape 1. ;
    Cp.Shape.set_friction shape 1. ;
    Cp.Shape.set_layers shape not_grabable_mask ;

    let shape = Cp.Space.add_shape space (Cp.Shape.make_segment rogue_box_body c d 0.) in
    Cp.Shape.set_elasticity shape 1. ;
    Cp.Shape.set_friction shape 1. ;
    Cp.Shape.set_layers shape not_grabable_mask ;

    let shape = Cp.Space.add_shape space (Cp.Shape.make_segment rogue_box_body d a 0.) in
    Cp.Shape.set_elasticity shape 1. ;
    Cp.Shape.set_friction shape 1. ;
    Cp.Shape.set_layers shape not_grabable_mask ;

    let mass = 1. in
    let width = 60. in
    let height = 60. in

    for i=0 to 2
    do
      for j=0 to 6
      do
        let body = Cp.Space.add_body space (Cp.Body.make mass (Ochipmunk.Cp.moment_for_box mass width height)) in
        Cp.Body.set_pos body (Cp.Vector.make (float (i*60 - 150)) (-.float (j*30 - 150))) ;
        let shape = Cp.Space.add_shape space (Cp.Shape.make_box body width height) in
        Cp.Shape.set_elasticity shape 0. ;
        Cp.Shape.set_friction shape 0.7 ;        
      done
    done ;
    space

  method update ticks space =
    let steps = 3 in
    let dt = 1./.60./. float steps in
    for i = 0 to steps - 1
    do
      Cp.Body.update_position rogue_box_body dt ;
      Cp.Space.step space dt 
    done

  method draw target space =
    default_draw_impl target space

  method destroy = ()
end
