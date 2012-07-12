open CpShapeType.PolyShapeImpl
open CpSplittingPlane

module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  open Type

  module NearestPointQueryInfo = CpNearestPointQueryInfo.Make(Param)
  module SegmentQueryInfo = CpSegmentQueryInfo.Make(Param)


  let value_on_axis poly n d =
    let verts = poly.t_verts in
    let m = Array.fold_left (fun m vert -> min m (CpVector.dot n vert) ) infinity verts in
    m -. d

  let contains_vert poly v =
    let planes = poly.t_planes in
    let num_verts = Array.length poly.verts in
    let rec impl i =
      if i < num_verts
      then 
        let dist = compare planes.(i) v in
        if dist > 0.
        then false
        else impl (i+1)
      else true
    in impl 0

  let contains_vert_partial poly v n =
    let planes = poly.t_planes in
    let num_verts = Array.length poly.verts in
    let rec impl i =
      if i < num_verts
      then
        if CpVector.dot planes.(i).n n >= 0. && compare planes.(i) v > 0.
        then false
        else impl (i+1)
      else true
    in impl 0

  let transform_verts poly p rot =
    let l = ref     infinity in
    let r = ref neg_infinity in
    let b = ref     infinity in
    let t = ref neg_infinity in
    Array.iteri (fun i vert -> CpVector.(
      let v = add p (rotate vert rot) in
      poly.t_verts.(i) <- v ;
      l := min !l v.x ;
      r := max !r v.x ;
      b := min !b v.y ;
      t := max !t v.y
    ) ) poly.verts ;
    CpBB.make !l !b !r !t

  let transform_axes poly p rot =
    Array.iteri (fun i plane -> CpVector.(
      let n = rotate plane.n rot in
      let d = dot p n +. plane.d in
      poly.t_planes.(i) <- { n ; d }
    ) ) poly.planes

  let cache_data poly shape p rot =
    transform_axes poly p rot ;
    shape.shbb <- transform_verts poly p rot ;
    shape.shbb

  let nearest_point_query poly shape p =
    let count = Array.length poly.verts in
    let planes = poly.t_planes in
    let verts = poly.t_verts in

    let v0 = ref verts.(count - 1) in
    let min_dist = ref infinity in
    let closest_point = ref CpVector.zero in
    let outside = ref false in

    Array.iteri (fun i v1 -> begin
      if CpSplittingPlane.compare planes.(i) p > 0. then outside := true ;
      let closest = CpVector.closest_point_on_segment p !v0 v1 in

      let dist = CpVector.dist p closest in
      if dist < !min_dist
      then ( min_dist := dist ; closest_point := closest ) ;
      v0 := v1
    end ) verts ;
    NearestPointQueryInfo.({ 
      shape = Some shape; 
      p = !closest_point ;
      d = if !outside then !min_dist else -. !min_dist
    })

  let segment_query poly shape a b =
    let axes = poly.t_planes in
    let verts = poly.t_verts in
    let num_verts = Array.length poly.verts in
    let info = ref SegmentQueryInfo.({ shape = None ; t = 0. ; n = CpVector.zero }) in

    Array.iteri (fun i axe -> 
      let n = axe.n in
      let an = CpVector.(dot a n) in
      if axe.d <= an
      then 
        let bn = CpVector.(dot b n) in
        let t = (axe.d -. an) /. (bn -. an) in
        if t >= 0. && t <= 1.
        then CpVector.(
          let point = lerp a b t in
          let dt = -. (cross n point) in
          let dt_min = -. (cross n verts.(i) ) in
          let dt_max = -. (cross n verts.( (i+1) mod num_verts )) in
          if dt_min <= dt && dt <= dt_max
          then info := SegmentQueryInfo.({ shape = Some shape ; t ; n })
        )
    ) axes ;
    !info

  let validate verts =
    let length = Array.length verts in
    let rec validate_impl array i =
      if i < length 
      then begin
        let a = verts.(i) in
        let b = verts.( (i+1) mod length ) in
        let c = verts.( (i+2) mod length ) in
        if CpVector.(cross (sub b a) (sub c a)) > 0.
        then false
        else validate_impl array (i+1)
      end
      else true
    in validate_impl verts 0

  let get_num_verts poly =
    Array.length poly.verts

  let get_vert poly idx =
    assert (0 <= idx && idx < (get_num_verts poly)) ;
    poly.verts.(idx)

  let set_up_verts verts offset =
    assert (validate verts) ;
    let num_verts = Array.length verts in
    let dummy_plane = { n = CpVector.zero ; d = 0. } in
    let poly = 
      { 
        verts = Array.make num_verts CpVector.zero ;
        planes = Array.make num_verts dummy_plane ;
        t_verts = Array.make num_verts CpVector.zero ;
        t_planes = Array.make num_verts dummy_plane ;
      } in
    Array.iteri (fun i vert -> CpVector.(
      let a = add offset vert in
      let b = add offset verts.( (i+1) mod num_verts ) in
      let n = normalize (perp (sub b a)) in

      poly.verts.(i) <- a ;
      poly.planes.(i) <- { n ; d = dot n a }
    ) ) verts ;
    poly


  let make =
    set_up_verts
end
