open OChipmunkDemo


let pi = 4. *. atan 1.

class plink =
object
  inherit demo "Plink"

  method init =
    let space = Cp.Space.make () in
    Cp.Space.set_iterations space 5 ;
    Cp.Space.set_gravity space (Cp.Vector.make 0. 100.);

    let static_body = Cp.Space.get_static_body space in

    let num_verts = 5 in
    let verts = Array.init num_verts (fun i ->
      let angle = -.2. *. pi *. float i /. float num_verts in
      Cp.Vector.make (10. *. cos angle) (10. *. sin angle)
    ) in

    let tris = Cp.Vector.([| make 15. 15. ; make 0. (-.10.) ; make (-.15.) 15. |]) in

    for i = 0 to 8
    do
      for j = 0 to 5
      do
        let stagger = (j mod 2) * 40 in
        let offset = Cp.Vector.make (float (i*80 - 320 + stagger)) (float (240 - j*70)) in
        let shape = Cp.Space.add_shape space (Cp.Shape.make_poly static_body tris offset) in
        Cp.Shape.set_elasticity shape 1. ;
        Cp.Shape.set_friction shape 1. ;
        Cp.Shape.set_layers shape not_grabable_mask ;
      done
    done ;

    for i = 0 to 299
    do
      let body = Cp.Space.add_body space (Cp.Body.make 1. (Ochipmunk.Cp.moment_for_poly 1. verts Cp.Vector.zero)) in
      let x = (Random.float 640.) -. 320. in
      Cp.Body.set_pos body (Cp.Vector.make x (-.350.)) ;

      let shape = Cp.Space.add_shape space (Cp.Shape.make_poly body verts Cp.Vector.zero) in
      Cp.Shape.set_elasticity shape 0. ;
      Cp.Shape.set_friction shape 0.4 ;
    done ;
    space

  method update ticks space =
    let steps = 1 in
    let dt = 1./.60./. float steps in
    for i = 0 to steps - 1
    do 
      Cp.Space.step space dt ;
      Cp.Space.each_body space (fun body ->
        let pos = Cp.Body.get_pos body in
        if Cp.Vector.(pos.y) > 260. || abs_float Cp.Vector.(pos.x) > 340.
        then let x = (Random.float 640.) -. 320. in
             Cp.Body.set_pos body (Cp.Vector.make x (-.260.))
      )
    done

  method draw target space =
    default_draw_impl target space

  method destroy = ()
end
