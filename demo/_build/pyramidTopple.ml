open OChipmunkDemo
open Ochipmunk

let width = 4.
let height = 30.

let add_domino space pos flipped =
  let mass = 1. in
  let moment = Cp.moment_for_box mass width height in
  let body = CpSpace.add_body space (CpBody.make mass moment) in
  CpBody.set_pos body pos ;

  let shape = 
    if flipped 
    then CpShape.make_box body height width
    else CpShape.make_box body width height
  in
  CpSpace.add_shape space shape ;
  CpShape.set_elasticity shape 0. ;
  CpShape.set_friction shape 0.6


class pyramid_topple =
object
  inherit demo "Pyramid Topple"

  method init =
    let space = CpSpace.make () in
    CpSpace.set_iterations space 30 ;
    CpSpace.set_gravity    space (CpVector.make 0. 300.) ;
    CpSpace.set_sleep_time_threshold space 0.5 ;
    CpSpace.set_collision_slop space 0.5 ;

    let shape = CpShape.make_segment (CpSpace.get_static_body space) (CpVector.make (-.600.) 240.) (CpVector.make 600. 240.) 0. in
    CpSpace.add_shape space shape ;
    CpShape.set_elasticity shape 1. ;
    CpShape.set_friction shape 1. ;
    CpShape.set_layers shape not_grabable_mask ;

    let n = 12 in
    for i = 0 to (n-1)
    do
      for j = 0 to (n-i-1)
      do
        let offset = CpVector.make 
          ((float j -. float (n - 1 - i) *. 0.5) *. 1.5 *. height)
          (-.((float i +. 0.5) *. (height +. 2. *. width) -. width -. 240.))
        in
        add_domino space offset false ;
        add_domino space CpVector.(add offset (make 0. ((-. height -. width)/.2.))) true ;

        if j = 0
        then add_domino space CpVector.(add offset (make (0.5*.(width -. height)) (-. height -. width))) false ;

        if j <> (n - i - 1)
        then add_domino space CpVector.(add offset (make (height*.0.75) ((-.height -. 3.*.width)/.2.))) true
        else add_domino space CpVector.(add offset (make (0.5*.(height -. width)) (-.height -. width))) false ;
      done
    done ;
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
