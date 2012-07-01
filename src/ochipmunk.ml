module Cp =
struct   
  let version_major = 3
  let version_minor = 1
  let version_release = 1
    
  let pi = 4.0 *. atan 1.0;;
  
  let message condition file line isError message format =
    if isError then Printf.eprintf "Aborting due to Chipmunk error: " 
    else Printf.eprintf "Chipmunk warning: " ;
    Printf.eprintf "\tFailed condition: %s\n" condition ;
    Printf.eprintf "\tSource:%s:%d\n" file line ;  
    Printf.eprintf message format    

  let version_string = Printf.sprintf "%l.%l.%l" version_major version_minor version_release

  let moment_for_circle m r1 r2 offset =
    m *. (0.5 *. (r1 *. r1 +. r2 *. r2) +. CpVector.lengthsq offset)

  let area_for_circle r1 r2 =
    pi *. abs_float(r1 *. r1 -. r2 *. r2)

  let moment_for_segment m a b =
    let offset = CpVector.mult (CpVector.add a b) 0.5 in
    m *. ((CpVector.distsq b a) /. 12. +. CpVector.lengthsq offset)

  let area_for_segment a b r =
    r *. (pi *. r +. 2. *. (CpVector.dist a b))

  let moment_for_poly m verts offset =
    let num_verts = Array.length verts in
    let sum1 = ref 0. in
    let sum2 = ref 0. in
    Array.iteri (fun i e -> begin
      let v1 = CpVector.add e offset in
      let v2 = CpVector.add verts.((i+1) mod num_verts) offset in

      let a = CpVector.cross v2 v1 in
      let b = (CpVector.dot v1 v1) +. (CpVector.dot v1 v2) +. (CpVector.dot v2 v2) in

      sum1 := !sum1 +. a *. b ;
      sum2 := !sum2 +. a
    end ) verts ;
    (m *. !sum1) /. (6. *. !sum2)


  let area_for_poly verts =
    let num_verts = Array.length verts in
    let area = ref 0. in
    Array.iteri (fun i v1 -> let v2 = verts.((i+1) mod num_verts) in
			     area := !area +. (CpVector.cross v1 v2)) verts ;
    -. !area /. 2.

  let centroid_for_poly verts =
    let num_verts = Array.length verts in
    let sum = ref 0. in
    let vsum = ref CpVector.zero in
    Array.iteri (fun i v1 -> begin
      let v2 = verts.((i+1) mod num_verts) in
      let cross_ = CpVector.cross v1 v2 in
      sum := !sum +. cross_ ;
      vsum := CpVector.(add !vsum (mult (add v1 v2) cross_))
    end ) verts ;
    CpVector.mult !vsum (1. /. (3. *. !sum))


  let recenter_poly verts =
    let centroid = centroid_for_poly verts in
    Array.iteri (fun i v -> verts.(i) <- CpVector.sub v centroid) verts


  let moment_for_box m width height =
    m *. (width *. width +. height *. height) /. 12.

  let moment_for_box2 m box =
    let open CpBB in
	let width = box.r -. box.l in
	let height = box.t -. box.b in
	let offset = CpVector.mult (CpVector.make (box.l +. box.r) (box.b +. box.t)) 0.5 in
	(moment_for_box m width height) +. m *. (CpVector.lengthsq offset)

  let loop_indexes verts =
    let start = ref 0 in
    let end_ = ref 0 in
    let min = ref verts.(0) in
    let max = ref !min in
    Array.iteri( fun i v -> begin
      let open CpVector in
	  if v.x < !min.x || (v.x == !min.x && v.y < !min.y)
	  then (min := v ; start := i)
	  else if v.x > !max.x || (v.x == !max.x && v.y > !max.y)
	  then (max := v ; end_ := i)
    end ) verts ;
    (!start, !end_)




  let q_hull_partition verts offset count a b tol =
    let elem a i = a.(offset + i) in
    
    let swap_array_elem a i1 i2 =
      let tmp = elem a i1 in
      a.(offset + i1) <- elem a i2 ;
      a.(offset + i2) <- tmp
    in

    if count = 0
    then 0
    else begin
      let max = ref 0. in
      let pivot = ref 0 in
      
      let delta = CpVector.sub b a in
      let valueTol = tol *. (CpVector.length delta) in
      
      let head = ref 0 in
      let tail = ref 0 in
      while !head <= !tail 
      do
	let value = CpVector.(cross delta (sub (elem verts !head) a)) in
	if value > valueTol
	then begin 
	  if value > !max
	  then ( max := value ; pivot := !head ) ;
	  incr head
	end
	else ( swap_array_elem verts !head !tail ; decr tail )
      done ;
      if !pivot <> 0 then swap_array_elem verts 0 !pivot ;
      !head
    end


  let rec q_hull_reduce tol verts offset count a pivot b result resultOffset =
    let count = Array.length verts in
    if count < 0 
    then 0
    else if count = 0
    then ( result.(resultOffset) <- pivot ; 1 )
    else begin
      let left_count = q_hull_partition verts offset count a pivot tol in
      let index = 1 + (q_hull_reduce tol verts (offset+1) (left_count - 1) a verts.(offset) pivot result resultOffset) in
      result.(resultOffset + index - 1) <- pivot ;
      let right_count = q_hull_partition verts (offset+left_count) (count-left_count) pivot b tol in
      index + q_hull_reduce tol verts (offset + left_count + 1) (right_count - 1) pivot verts.(offset + left_count) b result (resultOffset + index)
    end
      
  let convex_hull verts ~result ~first tol =
    let swap_array_elem a i1 i2 =
      let tmp = a.(i1) in
      a.(i1) <- a.(i2) ;
      a.(i2) <- tmp
    in

    let fill_first i =
      match first with
	| Some rfirst -> rfirst := i
	| None -> ()
    in
    

    let count = Array.length verts in    
    let result = match result with
      | Some r -> (Array.blit verts 0 r 0 count ; r)
      | None -> verts
    in
    let (start,end_) = loop_indexes verts in
    if start = end_
    then ( fill_first 0; 1 )
    else begin
      swap_array_elem result 0 start ;
      swap_array_elem result 1 (if end_ = 0 then start else end_) ;
      let a = result.(0) in
      let b = result.(1) in
      fill_first start ;
      let result_count = (q_hull_reduce tol result 2 (count - 2) a b a result 1) + 1 in
      (* assert_soft (poly_validate resultCount) 
	 "Internal error: convex_hull and poly_validate did not agree" ^
	 "Please report this error with as much info as you can" *)
      result_count
    end
end
