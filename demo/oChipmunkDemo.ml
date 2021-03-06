open OcsfmlWindow
open OcsfmlGraphics
open Cp.Type

class virtual demo (n:string) =
object
  method name = n
  method virtual init : Cp.Space.t
  method virtual update : int -> Cp.Space.t -> unit
  method virtual draw : render_window -> Cp.Space.t -> unit
  method virtual destroy : unit
end


let time = ref 0.
let mouse = ref Cp.Vector.zero
let right_click = ref false
let message_string = ref ""
let keyboard = ref Cp.Vector.zero
let demos : demo array ref = ref [||]

let width = 640
let height = 480

let grabable_mask_bit = 1 lsl 31
let not_grabable_mask = lnot grabable_mask_bit


type t =
    {
      demo_index      :int        ;
      mutable paused          :bool       ;
      mutable step            :bool       ;
      mutable draw_bbs        :bool       ;
              space           :Cp.Space.t  ;
      mutable ticks           :int        ;
      
      mutable mouse_body      :Cp.Body.t ;
      mutable mouse_joint     :Cp.Constraint.t option ; 

      mutable key_up          :bool       ;
      mutable key_down        :bool       ;
      mutable key_left        :bool       ;
      mutable key_right       :bool       ;
      mutable max_arbiters    :int        ;
      mutable max_points      :int        ;
      mutable max_constraints :int        ;
      mutable scale           :float      ;
              info_view       :view       ;
              scene_view      :view       ;
      target          :render_window
    }
      
      

let default_draw_impl target space = OChipmunkDebugDraw.(
  draw_shapes target space ;
(*  draw_constraints target space ; *)
  draw_collision_points target space
)

let draw_string (target:#render_target) x y string =
  let tpl = String.make 1 Char.(chr (code 'A' + Array.length (!demos) - 1)) in
  let string = Str.global_replace (Str.regexp "\\*") tpl string in
  let to_draw = new text ~string ~position:(x,y) ~character_size:10 () in
  target#draw to_draw

let draw_instructions target =
  draw_string target (-.300.)  (-.220.)
    ("Controls:\n" ^
        "A - * Switch demos. (return restarts)\n" ^
        "Use the mouse to grab objects.\n" ^
        "= toggles bounding boxes." )



let draw_info info =
  let arbiters = List.length info.space.sparbiters in
  let points = List.fold_left 
      (fun p arb -> p + CpArray.length arb.acontacts) 0 info.space.sparbiters in
  let constraints = (List.length info.space.spconstraints + points) * info.space.spiterations in
  info.max_arbiters <- max arbiters info.max_arbiters ;
  info.max_points <- max points info.max_points ;
  info.max_constraints <- max constraints info.max_constraints ;

  let bodies = info.space.spbodies in
  let ke_calc = fun ke body -> 
    if body.bm = infinity || body.bi = infinity
    then ke
    else ke +. body.bm*.Cp.Vector.dot body.bv body.bv +. body.bi *. body.bw *. body.bw
  in
  let ke = List.fold_left ke_calc 0. bodies in
  let buffer = Printf.sprintf "Arbiters: %d (%d) - Contact Ponts: %d (%d)\nOther constraints: %d, Iterations: %d\nConstraints x Iteration: %d (%d)Time:%5.2fs, KE:%5.2e"
    arbiters info.max_arbiters points info.max_points
    (List.length info.space.spconstraints) info.space.spiterations
    constraints info.max_constraints
    !time (if ke < 1e-10 then 0. else ke)
  in
  draw_string info.target 0. (-.220.) buffer


let update_view view width height =
  let scale = min (float width /. 640.) (float height /. 480.) in
  let w = (640. *. scale) /. float width  in
  let h = (480. *. scale) /. float height in
  let viewport =
    let left = (1. -. w)/.2. in
    let top  = (1. -. h)/.2. in
    { left ; top ; width = w ; height = h }
  in
  view#set_viewport viewport


let reshape info width height =
  update_view info.info_view width height ;
  update_view info.scene_view width height

let draw_shape_bb info shape =
  OChipmunkDebugDraw.draw_bb info.target shape.shbb (Color.rgba 77 123 77 255)

let display info =
  let target = info.target in
  target#clear ~color:(Color.rgba 52 62 72 255) () ; 


  target#set_view info.scene_view ;
  !demos.(info.demo_index)#draw target info.space ;

  if not info.paused || info.step
  then begin
    let new_point = Cp.Vector.lerp info.mouse_body.bp !mouse 0.25 in
    info.mouse_body.bv <- Cp.Vector.(mult (sub new_point info.mouse_body.bp) 60.) ;
    info.mouse_body.bp <- new_point ;

    !demos.(info.demo_index)#update info.ticks info.space ;
    info.ticks <- info.ticks + 1 ;
    time := (float info.ticks) /. 60. ;
    info.step <- false
  end ;
  
  if info.draw_bbs then Cp.Space.each_shape info.space (draw_shape_bb info) ;
  
  target#set_view info.info_view ;
  draw_instructions target ;
  draw_info info ;
  draw_string target (-.300.) 200. !message_string ;

  target#display



let demo_title index =
  let ltr = Char.(chr (code 'a' + index)) in
  Printf.sprintf "Demo(%c): %s" ltr !demos.(index)#name

let run_demo index target = 
  target#set_title (demo_title index) ;
  {
    demo_index      = index ;
    paused          = false ;
    step            = false ;
    draw_bbs        = false ;
    space           = !demos.(index)#init  ;
    ticks           = 0     ;
    mouse_body      = Cp.Body.make infinity infinity ; 
    mouse_joint     = None  ;
    key_up          = false ;
    key_down        = false ;
    key_left        = false ;
    key_right       = false ;
    max_arbiters    = 0     ;
    max_points      = 0     ;
    max_constraints = 0     ;
    scale           = 1.    ;
    info_view       = new view (`Center ((0.,0.),(float width, float height))) ;
    scene_view      = new view (`Center ((0.,0.),(float width, float height))) ;
    target          = target
  }
    

let index_of_keycode key =
  KeyCode.(
    match key with
      | A -> 0
      | B -> 1
      | C -> 2
      | D -> 3
      | E -> 4
      | F -> 5      
      | G -> 6
      | H -> 7
      | I -> 8
      | J -> 9
      | K -> 10
      | L -> 11
      | M -> 12
      | N -> 13
      | O -> 14
      | P -> 15
      | Q -> 16
      | R -> 17
      | S -> 18
      | T -> 19
      | U -> 20
      | V -> 21
      | W -> 22
      | X -> 23
      | Y -> 24
      | Z -> 25
      | _ -> -1
  )
    

    
    
let key_pressed info key =
  let idx = index_of_keycode key in
  if idx >= 0 && idx < Array.length !demos
  then (!demos.(info.demo_index)#destroy ; run_demo idx info.target)
  else if key = KeyCode.Return
  then (!demos.(info.demo_index)#destroy ; run_demo info.demo_index info.target)
  else KeyCode.(
    let translate_increment = 50. /. info.scale in
    let scale_increment = 1.2 in
    let view = info.scene_view  in
    (match key with
        | Pause -> info.paused <- not info.paused
        | Numpad1 -> info.step <- true
        | Equal -> info.draw_bbs <- not info.draw_bbs
        | BackSlash -> ()
        | Numpad5 -> begin
          view#set_center 0. 0. ; 
          view#set_size (float width/.2.) (float height/.2.) ;
          info.scale <- 1.
        end
        | Numpad4 -> view#move (-.translate_increment) 0. 
        | Numpad6 -> view#move    translate_increment  0.
        | Numpad2 -> view#move 0.    translate_increment
        | Numpad8 -> view#move 0. (-.translate_increment) 
        | Numpad7 -> begin
          view#zoom (1./.scale_increment) ;
          info.scale <- info.scale /. scale_increment
        end
        | Numpad9 -> begin
          view#zoom scale_increment ; 
          info.scale <- info.scale *. scale_increment
        end 
        | _ -> ()
    );
    info
  )

let mouse_to_space info x y =
  let (xc,yc) = info.target#convert_coords (x,y) in
  Cp.Vector.make xc yc

let mouse info x y =
  mouse := mouse_to_space info x y

let pressed info button x y =
  match button with
    | Event.LeftButton -> begin
        let point = mouse_to_space info x y in
        let sh = Cp.Space.point_query_first info.space point grabable_mask_bit Cp.Shape.no_group in
        match sh with
          | Some shape -> begin
            let body = shape.shbody in
            let joint = Cp.PivotJoint.make2 info.mouse_body body Cp.Vector.zero (Cp.Body.world2local body point) in
            joint.cmax_force <- 50000. ;
            joint.cerror_bias <- (1. -. 0.15) ** 60. ;
            info.mouse_joint <- Some (Cp.Space.add_constraint info.space joint)
          end
          | _ -> ()
    end
    | Event.RightButton -> right_click := true
    | _ -> ()

let released info button x y =
  match button with
    | Event.LeftButton -> CpOption.do_if (fun con -> begin
      Cp.Space.remove_constraint info.space con ;
      info.mouse_joint <- None
    end) info.mouse_joint
    | Event.RightButton -> right_click := false
    | _ -> ()

let set_arrow_direction info =
  let x = (if info.key_right then 1. else 0.) -. (if info.key_left then 1. else 0.) in
  let y = (if info.key_down  then 1. else 0.) -. (if info.key_up   then 1. else 0.) in
  keyboard := Cp.Vector.make x y

let arrow_key_pressed info key =
  KeyCode.(
    match key with
      | Up -> info.key_up <- true
      | Down -> info.key_down <- true
      | Left -> info.key_left <- true
      | Right -> info.key_right <- true
      | _ -> ()
  ) ;
  set_arrow_direction info

let arrow_key_released info key =
  KeyCode.(
    match key with
      | Up -> info.key_up <- false
      | Down -> info.key_down <- false
      | Left -> info.key_left <- false
      | Right -> info.key_right <- false
      | _ -> ()
  ) ;
  set_arrow_direction info

let init () =
  let window = new render_window (VideoMode.create ~w:640 ~h:480 ()) "" in
  window#set_framerate_limit 60 ;
  window#set_key_repeat_enabled false ;
  window



let rec main_loop info =
  let window = info.target in  
  let handle_key_pressed info c =
    KeyCode.(match c with
      | Up | Down | Left | Right -> (arrow_key_pressed info c ; info)
      | _ -> (key_pressed info c)
    )
  in
  let handle_key_released info c =
    KeyCode.(match c with
      | Up | Down | Left | Right -> (arrow_key_released info c)
      | _ -> ()
    )
  in
  
  let handle_event info e =
    Event.(match e with
      | KeyPressed { code ; _ } -> handle_key_pressed info code
      | _ -> begin
        (match e with
          | Closed -> window#close
          | Resized { width ; height } -> reshape info width height
          | MouseMoved { x ; y } -> mouse info x y
          | MouseButtonPressed (button,{x ; y}) -> pressed info button x y
          | MouseButtonReleased (button,{x ; y}) -> released info button x y
          | KeyReleased { code ; _ } -> handle_key_released info code
          | _ -> ()
        ) ;
        info
      end
    )
  in

  let rec event_loop info =
    match window#poll_event with
      | Some e -> 
          let info = handle_event info e in
          event_loop info
      | None -> info
  in

  if window#is_open 
  then begin
    let info = event_loop info in
    display info ;
    main_loop info
  end
