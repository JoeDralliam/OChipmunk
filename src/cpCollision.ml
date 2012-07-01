open CpShapeType
open CpType
(* type collision_func = CpShape.shape -> CpShape.shape -> CpContact.t -> int  *)

let circle2circle_query p1 p2 r1 r2 arr =
  let mindist = r1 +. r2 in
  let delta = CpVector.sub p2 p1 in
  let distsq = CpVector.lengthsq delta in
  if distsq >= mindist *. mindist 
  then 0
  else begin
    let dist' = sqrt distsq in
    let div_by = if dist' > 0. then dist' else infinity in
    CpVector.(ignore ( 
      CpContact.init 
        CpArray.(get arr 0)
        (add p1 (mult delta (0.5 +. (r1 -. 0.5*.mindist) /. div_by)))
        (if dist' > 0. then mult delta (1./.dist') else make 0. 0.)
        (dist' -. mindist)
        0L
    ) );
    1
  end

let circle2circle (circ1,_) (circ2,_) arr =
  CircleImpl.(circle2circle_query circ1.tc circ2.tc circ1.r circ2.r arr)
  
let circle2segment (circle,_) (segment,_) con =
  let seg_a = SegmentImpl.(segment.ta) in
  let seg_b = SegmentImpl.(segment.tb) in
  let center = CircleImpl.( circle.tc) in

  let seg_delta = CpVector.sub seg_b seg_a in
  let closest_t = CpFloat.clamp01 CpVector.((dot seg_delta (sub center seg_a)) /. (lengthsq seg_delta)) in
  let closest = CpVector.(add seg_a (mult seg_delta closest_t)) in
  if (circle2circle_query center closest CircleImpl.(circle.r) SegmentImpl.(segment.r) con) <> 0
  then 
    let n = CpContact.(CpArray.(get con 0).n) in
    if SegmentImpl.(closest_t = 0. && CpVector.dot n segment.a_tangent < 0.) ||
       SegmentImpl.(closest_t = 1. && CpVector.dot n segment.b_tangent < 0.)
    then 0
    else 1
  else 0

let next_contact_point arr num_ptr =
  let index = !num_ptr in
  if index < max_contacts_per_arbiter
  then (incr num_ptr ; CpArray.get arr index )
  else CpArray.get arr (max_contacts_per_arbiter - 1)
    

let find_msa poly planes =
  let num = Array.length planes in
  let rec impl i m m_idx =
    if i < num
    then 
      let dist = CpSplittingPlane.(CpPolyShape.value_on_axis poly planes.(i).n planes.(i).d) in
      if dist > 0.
      then (-1, 0.)
      else begin
        let (m, m_idx) = if dist > m then (dist,i) else (m, m_idx) in
        impl (i+1) m m_idx
      end
    else (m_idx, m)
  in impl 0 neg_infinity 0

let find_verts_fallback arr (poly1,shape1) (poly2,shape2) n dist =
  let num = ref 0 in
  Array.iteri (fun i v ->
    if CpPolyShape.contains_vert_partial poly2 v CpVector.(neg n)
    then ignore (CpContact.init (next_contact_point arr num) v n dist (CpPrivate.hash_pair shape1.shhashid i))) PolyShapeImpl.(poly1.t_verts) ;
    
  Array.iteri (fun i v ->
    if CpPolyShape.contains_vert_partial poly1 v n
    then ignore (CpContact.init (next_contact_point arr num) v n dist (CpPrivate.hash_pair shape2.shhashid i))) PolyShapeImpl.(poly2.t_verts) ;
  !num

let find_verts arr (poly1,shape1) (poly2,shape2) n dist =
  let num = ref 0 in
  Array.iteri (fun i v ->
    if CpPolyShape.contains_vert poly2 v
    then ignore (CpContact.init (next_contact_point arr num) v n dist (CpPrivate.hash_pair shape1.shhashid i))) PolyShapeImpl.(poly1.t_verts) ;
    
  Array.iteri (fun i v ->
    if CpPolyShape.contains_vert poly1 v
    then ignore (CpContact.init (next_contact_point arr num) v n dist (CpPrivate.hash_pair shape2.shhashid i))) PolyShapeImpl.(poly2.t_verts) ;
  if !num <> 0 then !num else (find_verts_fallback arr (poly1,shape1) (poly2,shape2) n dist)

let poly2poly (poly1,shape1) (poly2,shape2) arr =
  let open PolyShapeImpl in
      let (mini1, min1) = find_msa poly2 (poly1.t_planes) in
      if mini1 = -1 
      then 0
      else
        let (mini2,min2) = find_msa poly1 (poly2.t_planes) in
        if mini2 = -1 
        then 0
        else
          if min1 > min2
          then find_verts arr (poly1,shape1) (poly2,shape2) CpSplittingPlane.(poly1.t_planes.(mini1).n) min1
          else find_verts arr (poly1,shape1) (poly2,shape2) CpVector.(neg CpSplittingPlane.(poly2.t_planes.(mini2).n)) min2


let seg_value_on_axis seg n d =
  let open SegmentImpl in
      let a = CpVector.(dot n seg.ta) -. seg.r in
      let b = CpVector.(dot n seg.tb) -. seg.r in
      (min a b) -. d

let find_points_behind_seg arr num (seg,_) (poly,shape) p_dist coef =
  let module Seg = SegmentImpl in
  let dta = CpVector.cross Seg.(seg.tn) Seg.(seg.ta) in
  let dtb = CpVector.cross Seg.(seg.tn) Seg.(seg.tb) in
  let n = CpVector.mult Seg.(seg.tn) coef in

  Array.iteri (fun i v -> CpVector.(
    if dot v n < (dot Seg.(seg.tn) Seg.(seg.ta)) *. coef +. Seg.(seg.r)
    then 
      let dt = cross Seg.(seg.tn) v in
      if dta >= dt && dt >= dtb
      then ignore (CpContact.init (next_contact_point arr num) v n p_dist CpPrivate.(hash_pair shape.shhashid i) )
  ) ) PolyShapeImpl.(poly.t_verts)
    
let seg2poly (seg,shape1) (poly,shape2) arr =
  let module Seg  = SegmentImpl in
  let module Poly =    PolyShapeImpl in
  let planes = Poly.(poly.t_planes) in

  let seg_d = CpVector.dot Seg.(seg.tn) Seg.(seg.ta) in
  let min_norm = (CpPolyShape.value_on_axis poly Seg.(seg.tn) seg_d) -. Seg.(seg.r) in
  let min_neg = (CpPolyShape.value_on_axis poly (CpVector.neg Seg.(seg.tn)) (-.seg_d)) -. Seg.(seg.r) in
  if min_neg > 0. || min_norm > 0.
  then 0
  else begin
    let num_planes = Array.length planes in
    let rec impl i poly_min mini =
      if i < num_planes
      then 
        let dist = CpSplittingPlane.(seg_value_on_axis seg planes.(i).n planes.(i).d) in
        if dist > 0.
        then (-1, 0.)
        else begin
          let (poly_min, mini) = if dist > poly_min then (dist,i) else (poly_min, mini) in
          impl (i+1) poly_min mini
        end
      else (mini, poly_min)
    in 
    let (mini, poly_min) = impl 0 neg_infinity 0 in
    if mini = -1
    then 0
    else
      let num = ref 0 in
      let poly_n = CpVector.neg CpSplittingPlane.(planes.(mini).n) in
      let va = CpVector.(add Seg.(seg.ta) (mult poly_n Seg.(seg.r))) in
      let vb = CpVector.(add Seg.(seg.tb) (mult poly_n Seg.(seg.r))) in
      if CpPolyShape.contains_vert poly va
      then ignore (CpContact.init (next_contact_point arr num) va poly_n poly_min (CpPrivate.hash_pair shape1.shhashid 0)) ;
      if CpPolyShape.contains_vert poly vb
      then ignore (CpContact.init (next_contact_point arr num) vb poly_n poly_min (CpPrivate.hash_pair shape1.shhashid 1)) ;

      if min_norm >= poly_min || min_neg >= poly_min
      then begin
        if min_norm > min_neg
        then find_points_behind_seg arr num (seg,shape1) (poly,shape2) min_norm 1.
        else find_points_behind_seg arr num (seg,shape1) (poly,shape2) min_neg (-.1.)
      end ;
      
      if !num = 0
      then begin
        let poly_a = Poly.(poly.t_verts.(mini)) in
        let poly_b = Poly.(poly.t_verts.( (mini+1) mod (Array.length poly.t_verts))) in
        if circle2circle_query Seg.(seg.ta) poly_a Seg.(seg.r) 0. arr <> 0 then 1
        else if circle2circle_query Seg.(seg.tb) poly_a Seg.(seg.r) 0. arr <> 0 then 1
        else if circle2circle_query Seg.(seg.ta) poly_b Seg.(seg.r) 0. arr <> 0 then 1
        else if circle2circle_query Seg.(seg.tb) poly_b Seg.(seg.r) 0. arr <> 0 then 1
        else 0
      end
      else !num
  end

let circle2poly (circ,shape1) (poly,shape2) con =
  let module Circle = CircleImpl in
  let module Poly   =   PolyShapeImpl in
  let planes = Poly.(poly.t_planes) in

  let num_planes = Array.length planes in
  let rec impl i m mini =
    if i < num_planes
    then 
      let dist = Circle.(CpSplittingPlane.(compare planes.(i) circ.tc) -. circ.r) in
      if dist > 0.
      then (-1, 0.)
      else begin
        let (m, mini) = if dist > m then (dist,i) else (m, mini) in
        impl (i+1) m mini
      end
    else (mini, m)
  in 
  let (mini, m) = impl 0 neg_infinity 0 in
  if mini = -1 
  then 0
  else begin
    let n = CpSplittingPlane.(planes.(mini).n) in
    let a = Poly.(poly.t_verts.(mini)) in
    let b = Poly.(poly.t_verts.( (mini+1) mod (Array.length poly.t_verts) )) in
    let dta = CpVector.cross n a in
    let dtb = CpVector.cross n b in
    let dt = CpVector.cross n Circle.(circ.tc) in

    if dt < dtb
    then circle2circle_query Circle.(circ.tc) b Circle.(circ.r) 0. con
    else if dt < dta
    then begin 
      ignore (CpContact.init 
        CpArray.(get con 0)
        CpVector.(sub Circle.(circ.tc) (mult n Circle.(circ.r +. m/.2.)))
        CpVector.(neg n)
        m
        0L) ;
      1
    end
    else circle2circle_query Circle.(circ.tc) a Circle.(circ.r) 0. con
  end

let collide_shapes a b arr =
  if not (CpShape.get_type a <= CpShape.get_type b)
  then Printf.eprintf "Collision shapes passed to cpCollideShape () are not sorted" ;
  match (a.shstrategy,b.shstrategy) with
    | (Circle  circ1, Circle circ2) -> circle2circle  (circ1, a) (circ2, b) arr
    | (Circle  circ , Segment  seg) -> circle2segment (circ , a) (seg  , b) arr
    | (Circle  circ , Poly    poly) -> circle2poly    (circ , a) (poly , b) arr 
    | (Segment seg  , Poly    poly) ->    seg2poly    (seg  , a) (poly , b) arr
    | (Poly    poly1, Poly   poly2) ->   poly2poly    (poly1, a) (poly2, b) arr
    | _ -> 0
