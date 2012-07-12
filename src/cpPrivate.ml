open CpType
module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  module CircleShape = CpCircleShape.Make(Param)
  module SegmentShape = CpSegmentShape.Make(Param)
  module PolyShape = CpPolyShape.Make(Param)
  open Type


  let assert_soft cond text =
    if not cond then Printf.eprintf text

  let hash_coef = 3344921057L
  let hash_pair a b = Int64.(logxor (mul (of_int a) hash_coef) (mul (of_int b) hash_coef) )

  let next_constraint node body =
    if node.ca == body then node.cnext_a else node.cnext_b

  let next_arbiter node body =
    if node.abody_a == body then node.athread_a.thnext else node.athread_b.thnext



(* SPACE *)
  module Space =
  struct
    let lookup_handler space a b =
      let handler = CpHashSet.find space.spcollision_handlers (hash_pair a b) (a,b) in
      assert (handler <> None) ;
      handler

    let uncache_arbiter space arb =
      let a = arb.aa and b = arb.ab in
      let shape_pair = (a,b) in
      let arb_hashid = hash_pair (a.shhashid) (b.shhashid) in
      ignore( CpHashSet.remove space.spcached_arbiters arb_hashid shape_pair );
      space.sparbiters <- List.filter (fun a -> a != arb) space.sparbiters
  end

  module Body =
  struct
    let foreach_constraint bdy func =
      let rec impl c =
        match c with
          | Some constr -> begin
            func constr ;
            impl (next_constraint constr bdy)
          end
          | None -> ()
      in impl bdy.bconstraint_list

    let foreach_arbiter bdy func =
      let rec impl a =
        match a with
          | Some arb -> begin
            func arb ;
            impl (next_arbiter arb bdy)
          end
          | None -> ()
      in impl bdy.barbiter_list

    let foreach_shape bdy func =
      let rec impl s =
        match s with
          | Some shape -> begin
            func shape ;
            impl shape.shnext
          end
          | None -> ()
      in impl bdy.bshape_list

    let foreach_component root func =
      let rec impl b =
        match b with
          | Some body -> begin
            func body ;
            impl ComponentNode.(body.bnode.next)
          end
          | None -> ()
      in impl root

    let eql : body' -> body' -> bool = (==)

    let is_sleeping body = 
      ComponentNode.(body.bnode.root) <> None
        
    let is_static body =
      ComponentNode.(body.bnode.idle_time) = infinity
      
    let is_rogue body =
      body.bspace = None

    let kinetic_energy body =
      let vsq = CpVector.dot body.bv body.bv in
      let wsq = body.bw *. body.bw in
      (if vsq > 0. then vsq *. body.bm else 0.) +. (if wsq > 0. then wsq *. body.bi else 0.)
  end

  module Shape =
  struct
    let dispatch shape cfunc sfunc pfunc =
      match shape.shstrategy with
        | CpShapeType.Circle c -> cfunc c shape
        | CpShapeType.Segment s -> sfunc s shape
        | CpShapeType.Poly p -> pfunc p shape
            
            
    let update shape pos rot =
      let impl = dispatch shape 
        CircleShape.cache_data SegmentShape.cache_data PolyShape.cache_data
      in
      shape.shbb <- impl pos rot ;
      shape.shbb

    let eql : shape' -> shape' -> bool = (==)
  end

  module Constraint =
  struct
    let eql : constraint' -> constraint' -> bool = (==) 
  end

end
