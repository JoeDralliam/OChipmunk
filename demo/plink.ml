open OChipmunkDemo
open Ochipmunk

let pi = 4. *. atan 1.

class plink =
object
  inherit demo "Plink"

  method init =
    let space = CpSpace.make () in
    CpSpace.set_iterations space 5 ;
    CpSpace.set_gravity space (CpVector.make 0. 100.);

    let static_body = CpSpace.get_static_body space in

    let num_verts = 5 in
    let verts = Array.init num_verts (fun i ->
      let angle = -.2. *. pi *. float i /. float num_verts in
      CpVector.make (10. *. cos angle) (10. *. sin angle)
    ) in

    let tris = CpVector.([| make 15. 15. ; make 0. (-.10.) ; make (-.15.) 15. |]) in


    for i = 0 to 8
    do
      for j = 0 to 5
      do
        let stagger = (j mod 2) * 40 in
        let offset = CpVector.make (float (i*80 - 320 + stagger)) (float (240 - j*70)) in
        let shape = CpSpace.add_shape space (CpShape.make_poly static_body tris offset) in
        CpShape.set_elasticity shape 1. ;
        CpShape.set_friction shape 1. ;
        CpShape.set_layers shape not_grabable_mask ;
      done
    done ;

    for i = 0 to 299
    do
      let body = CpSpace.add_body space (CpBody.make 1. (Cp.moment_for_poly 1. verts CpVector.zero)) in
      let x = (Random.float 640.) -. 320. in
      CpBody.set_pos body (CpVector.make x (-.350.)) ;

      let shape = CpSpace.add_shape space (CpShape.make_poly body verts CpVector.zero) in
      CpShape.set_elasticity shape 0. ;
      CpShape.set_friction shape 0.4 ;
    done ;
    space

  method update ticks space =
    let steps = 1 in
    let dt = 1./.60./. float steps in
    for i = 0 to steps - 1
    do 
      CpSpace.step space dt ;
      CpSpace.each_body space (fun body ->
        let pos = CpBody.get_pos body in
        if CpVector.(pos.y) > 260. || abs_float CpVector.(pos.x) > 340.
        then let x = (Random.float 640.) -. 320. in
             CpBody.set_pos body (CpVector.make x (-.260.))
      )
    done

  method draw target space =
    default_draw_impl target space

  method destroy = ()
end
