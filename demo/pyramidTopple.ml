open OChipmunkDemo


let width = 4.
let height = 30.

let add_domino space pos flipped =
  let mass = 1. in
  let moment = Ochipmunk.Cp.moment_for_box mass width height in
  let body = Cp.Space.add_body space (Cp.Body.make mass moment) in
  Cp.Body.set_pos body pos ;

  let shape = 
    if flipped 
    then Cp.Shape.make_box body height width
    else Cp.Shape.make_box body width height
  in
  Cp.Space.add_shape space shape ;
  Cp.Shape.set_elasticity shape 0. ;
  Cp.Shape.set_friction shape 0.6


class pyramid_topple =
object
  inherit demo "Pyramid Topple"

  method init =
    let space = Cp.Space.make () in
    Cp.Space.set_iterations space 30 ;
    Cp.Space.set_gravity    space (Cp.Vector.make 0. 300.) ;
    Cp.Space.set_sleep_time_threshold space 0.5 ;
    Cp.Space.set_collision_slop space 0.5 ;

    let shape = Cp.Shape.make_segment (Cp.Space.get_static_body space) (Cp.Vector.make (-.600.) 240.) (Cp.Vector.make 600. 240.) 0. in
    Cp.Space.add_shape space shape ;
    Cp.Shape.set_elasticity shape 1. ;
    Cp.Shape.set_friction shape 1. ;
    Cp.Shape.set_layers shape not_grabable_mask ;

    let n = 12 in
    for i = 0 to (n-1)
    do
      for j = 0 to (n-i-1)
      do
        let offset = Cp.Vector.make 
          ((float j -. float (n - 1 - i) *. 0.5) *. 1.5 *. height)
          (-.((float i +. 0.5) *. (height +. 2. *. width) -. width -. 240.))
        in
        add_domino space offset false ;
        add_domino space Cp.Vector.(add offset (make 0. ((-. height -. width)/.2.))) true ;

        if j = 0
        then add_domino space Cp.Vector.(add offset (make (0.5*.(width -. height)) (-. height -. width))) false ;

        if j <> (n - i - 1)
        then add_domino space Cp.Vector.(add offset (make (height*.0.75) ((-.height -. 3.*.width)/.2.))) true
        else add_domino space Cp.Vector.(add offset (make (0.5*.(height -. width)) (-.height -. width))) false ;
      done
    done ;
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
