open OChipmunkDemo
open Ochipmunk
open OcsfmlGraphics

let image_width = 188
let image_height = 35
let image_row_length = 24

let image_bitmap = [|
  15;-16;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;7;-64;15;63;-32;-2;0;0;0;0;0;0;0;
  0;0;0;0;0;0;0;0;0;0;0;31;-64;15;127;-125;-1;-128;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
  0;0;0;127;-64;15;127;15;-1;-64;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;-1;-64;15;-2;
  31;-1;-64;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;-1;-64;0;-4;63;-1;-32;0;0;0;0;0;0;
  0;0;0;0;0;0;0;0;0;0;1;-1;-64;15;-8;127;-1;-32;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;
  1;-1;-64;0;-8;-15;-1;-32;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;-31;-1;-64;15;-8;-32;
  -1;-32;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;7;-15;-1;-64;9;-15;-32;-1;-32;0;0;0;0;0;
  0;0;0;0;0;0;0;0;0;0;31;-15;-1;-64;0;-15;-32;-1;-32;0;0;0;0;0;0;0;0;0;0;0;0;0;
  0;0;63;-7;-1;-64;9;-29;-32;127;-61;-16;63;15;-61;-1;-8;31;-16;15;-8;126;7;-31;
  -8;31;-65;-7;-1;-64;9;-29;-32;0;7;-8;127;-97;-25;-1;-2;63;-8;31;-4;-1;15;-13;
  -4;63;-1;-3;-1;-64;9;-29;-32;0;7;-8;127;-97;-25;-1;-2;63;-8;31;-4;-1;15;-13;
  -2;63;-1;-3;-1;-64;9;-29;-32;0;7;-8;127;-97;-25;-1;-1;63;-4;63;-4;-1;15;-13;
  -2;63;-33;-1;-1;-32;9;-25;-32;0;7;-8;127;-97;-25;-1;-1;63;-4;63;-4;-1;15;-13;
  -1;63;-33;-1;-1;-16;9;-25;-32;0;7;-8;127;-97;-25;-1;-1;63;-4;63;-4;-1;15;-13;
  -1;63;-49;-1;-1;-8;9;-57;-32;0;7;-8;127;-97;-25;-8;-1;63;-2;127;-4;-1;15;-13;
  -1;-65;-49;-1;-1;-4;9;-57;-32;0;7;-8;127;-97;-25;-8;-1;63;-2;127;-4;-1;15;-13;
  -1;-65;-57;-1;-1;-2;9;-57;-32;0;7;-8;127;-97;-25;-8;-1;63;-2;127;-4;-1;15;-13;
  -1;-1;-57;-1;-1;-1;9;-57;-32;0;7;-1;-1;-97;-25;-8;-1;63;-1;-1;-4;-1;15;-13;-1;
  -1;-61;-1;-1;-1;-119;-57;-32;0;7;-1;-1;-97;-25;-8;-1;63;-1;-1;-4;-1;15;-13;-1;
  -1;-61;-1;-1;-1;-55;-49;-32;0;7;-1;-1;-97;-25;-8;-1;63;-1;-1;-4;-1;15;-13;-1;
  -1;-63;-1;-1;-1;-23;-49;-32;127;-57;-1;-1;-97;-25;-1;-1;63;-1;-1;-4;-1;15;-13;
  -1;-1;-63;-1;-1;-1;-16;-49;-32;-1;-25;-1;-1;-97;-25;-1;-1;63;-33;-5;-4;-1;15;
  -13;-1;-1;-64;-1;-9;-1;-7;-49;-32;-1;-25;-8;127;-97;-25;-1;-1;63;-33;-5;-4;-1;
  15;-13;-1;-1;-64;-1;-13;-1;-32;-49;-32;-1;-25;-8;127;-97;-25;-1;-2;63;-49;-13;
  -4;-1;15;-13;-1;-1;-64;127;-7;-1;-119;-17;-15;-1;-25;-8;127;-97;-25;-1;-2;63;
  -49;-13;-4;-1;15;-13;-3;-1;-64;127;-8;-2;15;-17;-1;-1;-25;-8;127;-97;-25;-1;
  -8;63;-49;-13;-4;-1;15;-13;-3;-1;-64;63;-4;120;0;-17;-1;-1;-25;-8;127;-97;-25;
  -8;0;63;-57;-29;-4;-1;15;-13;-4;-1;-64;63;-4;0;15;-17;-1;-1;-25;-8;127;-97;
  -25;-8;0;63;-57;-29;-4;-1;-1;-13;-4;-1;-64;31;-2;0;0;103;-1;-1;-57;-8;127;-97;
  -25;-8;0;63;-57;-29;-4;-1;-1;-13;-4;127;-64;31;-2;0;15;103;-1;-1;-57;-8;127;
  -97;-25;-8;0;63;-61;-61;-4;127;-1;-29;-4;127;-64;15;-8;0;0;55;-1;-1;-121;-8;
  127;-97;-25;-8;0;63;-61;-61;-4;127;-1;-29;-4;63;-64;15;-32;0;0;23;-1;-2;3;-16;
  63;15;-61;-16;0;31;-127;-127;-8;31;-1;-127;-8;31;-128;7;-128;0;0
                   |]       

let get_pixel x y =
  (image_bitmap.( (x lsr 3) + y * image_row_length ) lsr (- x land 0x7)) land 1

let make_ball x y =
  let body = CpBody.make 1. infinity in
  CpBody.set_pos body (CpVector.make x y) ;

  let shape = CpShape.make_circle body 0.95 CpVector.zero in
  CpShape.set_elasticity shape 0. ;
  CpShape.set_friction shape 0. ;
  shape

class logo_smash =
object
  inherit demo "Logo Smash"

  val mutable body_count = 0


  method init =
    let space = CpSpace.make () in
    CpSpace.set_iterations space 1 ;
    CpSpace.use_spatial_hash space 2. 10000 ;


    body_count <- 0 ;
    for y = 0 to (image_height - 1)
    do
      for x = 0 to (image_width - 1)
      do
        if get_pixel x y <> 0
        then begin
          let x_jitter = (Random.float 0.05) in
          let y_jitter = (Random.float 0.05) in

          let shape = make_ball 
            (2.*.(float (x - image_width /2) +. x_jitter))
            (2.*.(float (y - image_height/2) -. y_jitter))
          in
          CpSpace.add_body space (CpShape.get_body shape) ;
          CpSpace.add_shape space shape ;
          body_count <- body_count + 1
        end
      done
    done ;

    let body = CpSpace.add_body space (CpBody.make infinity infinity) in
    CpBody.set_pos body (CpVector.make (-.1000.) 10.) ;
    CpBody.set_vel body (CpVector.make 400. 0.) ;

    let shape = CpSpace.add_shape space (CpShape.make_circle body 8. CpVector.zero) in
    CpShape.set_elasticity shape 0. ;
    CpShape.set_friction shape 0. ;
    CpShape.set_layers shape not_grabable_mask ;
    body_count <- body_count + 1 ;
    space

  method update ticks space =
    let steps = 1 in
    let dt = 1./.60./. float steps in
    for i = 0 to steps - 1
    do CpSpace.step space dt done

  method draw target space =
    let cursor = ref 0 in
    let verts = Array.make body_count CpVector.zero in
    CpSpace.each_body space (fun body -> begin
      verts.(!cursor) <- CpBody.get_pos body ;
      incr cursor
    end) ;
    OChipmunkDebugDraw.draw_points target 3 verts (Color.rgba 200 210 230 255) ;
    OChipmunkDebugDraw.draw_collision_points target space

  method destroy = ()
end
