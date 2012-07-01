

type 'a leaf = CpBB.t * 'a
type 'a node = CpBB.t * 'a subtree * 'a subtree
and 'a subtree = 
  | Leaf of 'a leaf
  | Node of 'a node


let get_bb tree index obj =
  let bb = index.bb_func obj in
  match tree.velocity_func with
    | Some f -> CpBB.(
      let coef = 0.1 in
      let x = (bb.r -. bb.l) *. coef in
      let y = (bb.t -. bb.b) *. coef in
      
      let v = CpVector.mult (f obj) 0.1 in
      let (vx,vy) = CpVector.(v.x,v.y) in
      make (bb.l +. (min (-.x) vx)) (bb.b +. (min (-.y) vy)) (bb.r +. (max x vx)) (bb.t +. (max y vy))
    )
    | None -> bb
        
let get_tree index =
  match index with
    | Some { strategy = BBTree t ; _ } -> Some t
    | _ -> None
        
let get_root_if_tree index =
  match index with
    | Some { strategy = BBTree t ; _ } -> t.root
    | _ -> None
        
let get_stamp tree index =
  let dynamic_tree = get_tree index.dynamic_index in
  match dynamic_tree with
    | Some t -> t.stamp
    | None -> tree.stamp
        
let increment_stamp tree index =
  let dynamic_tree = get_tree index.dynamic_index in
  match dynamic_tree with
    | Some t -> t.stamp <- t.stamp + 1
    | None -> tree.stamp <- tree.stamp + 1
        
        
let subtree_bb : 'a. 'a subtree -> CpBB.t = function
  | Leaf (bb,_) -> bb
  | Node (bb,_) -> bb

let node_make a b : 'a. 'a subtree -> 'a subtree -> 'a node = 
  Node (CpBB.merge (subtree_bb a) (subtree_bb b),a,b)

let rec subtree_insert subtree leaf : 'a. 'a subtree option -> 'a leaf -> 'a subtree
  match subtree with
    | None -> (Leaf leaf)
    | Some Leaf other -> node_make (Leaf leaf) (Leaf other)
    | Some Node (_,a,b)-> begin
      let (cost_a,cost_b) =
        let cost_a_tmp = CpBB.area (subtree_bb b) +. CpBB.merged_area (node_bb a) (node_bb leaf) in
        let cost_b_tmp = CpBB.area (subtree_bb a) +. CpBB.merged_area (node_bb b) (node_bb leaf) in
        if cost_a_tmp = cost_b_tmp
        then (CpBB.proximity (node_bb a) (node_bb leaf), 
              CpBB.proximity (node_bb b) (node_bb leaf))
        else (cost_a_tmp, cost_b_tmp)
      in
      let (newA,newB) =
        if cost_b < cost_a
        then (a, subtree_insert (Some b) leaf tree)
        else (subtree_insert (Some a) leaf tree, b)
      in
      let newBB = CpBB.merge internal.bb leaf.bb in
      Node (newBB,newA,newB)
    end

let rec subtree_query subtree obj bb func =
  if CpBB.intersects (node_bb subtree) bb
  then match subtree with
    | Leaf (_,obj_lf) -> func obj_lf obj
    | Node (_,a,b) -> begin
      subtree_query a obj bb func ;
      subtree_query b obj bb func
    end

let rec subtree_segment_query subtree obj a b t_exit func =
  let open Node in
      match subtree with
        | Leaf (_,obj_lf) -> func obj obj_lf
        | InternalNode (_,a_nd,b_nd) -> begin
          let t_a = CpBB.segment_query (node_bb a_nd) a b in
          let t_b = CpBB.segment_query (node_bb b_nd) a b in
          if t_a < t_b
          then begin
            let t_exit = 
              if t_a < t_exit 
              then min t_exit (subtree_segment_query a_nd obj a b t_exit func)
              else t_exit
            in
            if t_b < t_exit 
            then min t_exit (subtree_segment_query b_nd obj a b t_exit func)
            else t_exit
          end
          else begin
            let t_exit = 
              if t_b < t_exit 
              then min t_exit (subtree_segment_query b_nd obj a b t_exit func)
              else t_exit
            in
            if t_a < t_exit 
            then min t_exit (subtree_segment_query a_nd obj a b t_exit func)
            else t_exit
          end
        end
            
            
            
let rec node_remove (bb,a,b) leaf =
  match (a,b) with
    | (Leaf l, _) when l == leaf -> b
    | (_, Leaf l) when l == leaf -> a
    | (Node a_nd, Node b_nd) -> Node (node_make (node_remove a leaf) (node_remove b leaf))
    | (Node a_nd, Leaf b_lf) -> Node (node_make (node_remove a_nd leaf) b_lf)
    | (Leaf a_lf, Node b_nd) -> Node (node_make a_lf (node_remove b_nd leaf))
    | _ -> Node (bb,a,b)
        
        
let subtree_remove subtree leaf =
  let not_found () = Printf.eprintf "Leaf not found, can't be deleted" in
  match subtree with
    | None -> None
    | Some Leaf l when l == leaf -> None 
    | Some Leaf _ -> (not_found () ; subtree)
    | Some Node n -> Some (node_remove n leaf)


let rec mark_leaf_query subtree leaf left tree func =
  if CpBB.intersects (node_bb leaf) (node_bb subtree)
  then begin
    match subtree with 
      | Leaf (_,obj_lf) as lf ->
          if left
          then pair_insert leaf lf tree
          else begin
            let leaf =
              if l.node.stamp < leaf.node.stamp
              then pair_insert l leaf tree
              else 
            func leaf.node.obj l.node.obj
          end
      | Node (_,a,b) ->
          Node (node_make (mark_leaf_query a leaf left tree func,
                           mark_leaf_query b leaf left tree func))
  end

let mark_leaf leaf tree idx static_root func =
  let open Node in
      if leaf.node.stamp = (get_stamp tree idx)
      then begin
        (match static_root with
          | Some rt -> mark_leaf_query rt leaf false tree func
          | None -> ()) ;

        let rec mark_cousin node =
          match (node_parent node) with
            | Some parent -> begin
              if node == parent.node.a
              then mark_leaf_query parent.node.b leaf true tree func
              else mark_leaf_query parent.node.a leaf false tree func ;
              mark_cousin (InternalNode parent)
            end
            | None -> ()
        in
        mark_cousin (Leaf leaf)
      end 
      else begin
        let rec iter_over_pairs p =
          match p with
            | Some pair ->
                if leaf == pair.thread_b.leaf
                then (func pair.thread_a.leaf.node.obj leaf.node.obj ; 
                      iter_over_pairs pair.thread_b.next)
                else (iter_over_pairs pair.thread_a.next)
            | None -> ()
        in
        iter_over_pairs leaf.node.pairs
      end

let rec mark_subtree subtree tree idx static_root func =
  let open Node in
      match subtree with
        | Leaf l -> mark_leaf l tree idx static_root func
        | InternalNode n -> begin
          mark_subtree n.node.a tree idx static_root func ;
          mark_subtree n.node.b tree idx static_root func
        end
