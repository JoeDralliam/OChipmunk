open CpSpatialIndexType

module Make 
  ( SIdx : 
    sig 
      val collide_static : 'a t -> 'a t -> ('a,'a) query_func -> unit 
    end
  ) =
struct
  open BBTreeImpl
  type 'a t = 'a CpSpatialIndexType.BBTreeImpl.t

  let node_parent node =
    let open Subtree in
        match node with
          | Node n -> n.parent 
          | Leaf l -> l.parent
              
  let node_set_parent node parent =
    let open Subtree in
        match node with
          | Node n -> n.parent <- parent
          | Leaf n -> n.parent <- parent
              
  let node_bb node =
    let open Subtree in
        match node with
          | Node n -> n.bb 
          | Leaf n -> n.bb

  let node_set_bb node bb =
    let open Subtree in
        match node with
          | Node n -> n.bb <- bb
          | Leaf n -> n.bb <- bb


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

  let thread_unlink thread =
    let open Subtree in
        let next = thread.next in
        let prev = thread.prev in
        
        (match next with
          | Some n -> begin
            if n.thread_a.leaf == thread.leaf
            then n.thread_a.prev <- prev
            else n.thread_b.prev <- prev
          end
          | None -> ()) ;
        
        (match prev with
          | Some p -> begin
            if p.thread_a.leaf == thread.leaf
            then p.thread_a.next <- next
            else p.thread_b.next <- next
          end
          | None -> thread.leaf.node.pairs <- next)
          
  let pairs_clear leaf tree =
    let open Subtree in
        let rec suppress_pair p =
          match p with 
            | Some pair -> begin
              let next = 
                if pair.thread_a.leaf == leaf
                then begin
                  let next = pair.thread_a.next in
                  thread_unlink pair.thread_b ;
                  next
                end
                else begin
                  let next = pair.thread_b.next in
                  thread_unlink pair.thread_a ;
                  next
                end
              in
              suppress_pair next 
            end
            | None -> ()
        in
        let pair = leaf.node.pairs in
        leaf.node.pairs <- None ;
        suppress_pair pair

  let pair_insert a b tree =
    let open Subtree in
        let nextA = a.node.pairs in
        let nextB = b.node.pairs in
        let pair = Some { thread_a = { prev = None ; leaf = a ; next = nextA } ;
                          thread_b = { prev = None ; leaf = b ; next = nextB } } in
        a.node.pairs <- pair ;
        b.node.pairs <- pair ;

        (match nextA with
          | Some nA -> 
              if nA.thread_a.leaf == a 
              then nA.thread_a.prev <- pair
              else nA.thread_b.prev <- pair
          | _ -> () ) ;
        (match nextB with
          | Some nB ->
              if nB.thread_a.leaf == b
              then nB.thread_a.prev <- pair
              else nB.thread_b.prev <- pair
          | _ -> ())

(* node : 'a internal_node ;
   value : 'a node *)
  let node_set_a node value =
    let open Subtree in
        node.node.a <- value ;
        node_set_parent value (Some node)

  let node_set_b node value =
    let open Subtree in
        node.node.b <- value ;
        node_set_parent value (Some node)
          
  let node_make tree a b =
    let open Subtree in
        let internal_node = 
          { bb = CpBB.merge (node_bb a) (node_bb b) ;
            parent = None ;
            node = { a ; b } } in
        node_set_parent a (Some internal_node) ;
        node_set_parent b (Some internal_node) ;
        Node internal_node
          
  let is_leaf node =
    let open Subtree in
        match node with 
          | Leaf _ -> true
          | Node _ -> false

  let node_other node child =
    let open Subtree in
        if node.node.a == child then node.node.b else node.node.a

  let node_replace_child parent child value tree =
    let open Subtree in
        if parent.node.a == child
        then node_set_a parent value
        else node_set_b parent value ;
        
        let rec merge_bb node =
          match node with
            | None -> ()
            | Some n -> begin
              n.bb <- CpBB.merge (node_bb n.node.a) (node_bb n.node.b) ;
              merge_bb n.parent
            end
        in
        merge_bb (Some parent)
                
  let rec subtree_insert subtree leaf tree =
    let open Subtree in
        match subtree with
          | None -> (Leaf leaf)
          | Some (Leaf subtree_leaf) -> node_make tree (Leaf leaf) (Leaf subtree_leaf)
          | Some (Node internal) ->
              let (cost_a,cost_b) =
                let cost_a_tmp = (CpBB.area (node_bb internal.node.b)) +. (CpBB.merged_area (node_bb internal.node.a) leaf.bb) in
                let cost_b_tmp = (CpBB.area (node_bb internal.node.a)) +. (CpBB.merged_area (node_bb internal.node.b) leaf.bb) in
                if cost_a_tmp = cost_b_tmp
                then (CpBB.proximity (node_bb internal.node.a) leaf.bb, 
                      CpBB.proximity (node_bb internal.node.b) leaf.bb)
                else (cost_a_tmp, cost_b_tmp)
              in
              if cost_b < cost_a
              then node_set_b internal (subtree_insert (Some internal.node.b) leaf tree)
              else node_set_a internal (subtree_insert (Some internal.node.a) leaf tree) ;
              internal.bb <- CpBB.merge internal.bb leaf.bb ;
              (Node internal)

  let rec subtree_query subtree obj bb func =
    let open Subtree in
        if CpBB.intersects (node_bb subtree) bb
        then match subtree with
          | Leaf leaf -> func obj leaf.node.obj
          | Node internal -> begin
            subtree_query internal.node.a obj bb func ;
            subtree_query internal.node.b obj bb func
          end

  let rec subtree_segment_query subtree obj a b t_exit func =
    let open Subtree in
        match subtree with
          | Leaf leaf -> func obj leaf.node.obj
          | Node internal -> begin
            let t_a = CpBB.segment_query (node_bb internal.node.a) a b in
            let t_b = CpBB.segment_query (node_bb internal.node.b) a b in
            if t_a < t_b
            then begin
              let t_exit = 
                if t_a < t_exit 
                then min t_exit (subtree_segment_query internal.node.a obj a b t_exit func)
                else t_exit
              in
              if t_b < t_exit 
              then min t_exit (subtree_segment_query internal.node.b obj a b t_exit func)
              else t_exit
            end
            else begin
              let t_exit = 
                if t_b < t_exit 
                then min t_exit (subtree_segment_query internal.node.b obj a b t_exit func)
                else t_exit
              in
              if t_a < t_exit 
              then min t_exit (subtree_segment_query internal.node.a obj a b t_exit func)
              else t_exit
            end
          end


  let subtree_remove subtree leaf tree =
    let open Subtree in
        if subtree = None
        then (assert (leaf = None) ; None)
        else 
          let subtree = CpOption.(!? subtree) in
          let leaf = CpOption.(!? leaf) in
          match subtree with
            | Leaf l -> 
                if l == leaf
                then None
                else begin
                  let parent = CpOption.(!? (leaf.parent)) in
                  node_replace_child CpOption.(!? (parent.parent)) (Node parent) (node_other parent (Leaf leaf)) tree ;
                  Some subtree
                end
            | Node i ->
                let parent = CpOption.(!? (leaf.parent)) in
                if parent == i 
                then begin
                  let other = node_other i (Leaf leaf) in
                  (node_set_parent other i.parent);
                  Some other
                end
                else begin
                  node_replace_child CpOption.(!? (parent.parent)) (Node parent) (node_other parent (Leaf leaf)) tree ;
                  Some subtree
                end

  let rec mark_leaf_query subtree leaf left tree func =
    let open Subtree in
        if CpBB.intersects leaf.bb (node_bb subtree)
        then begin
          match subtree with 
            | Leaf l ->
                if left
                then pair_insert leaf l tree
                else begin
                  if l.node.stamp < leaf.node.stamp
                  then pair_insert l leaf tree ;
                  func leaf.node.obj l.node.obj
                end
            | Node i ->
                mark_leaf_query i.node.a leaf left tree func ;
                mark_leaf_query i.node.b leaf left tree func 
        end

  let mark_leaf leaf tree idx static_root func =
    let open Subtree in
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
                mark_cousin (Node parent)
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
    let open Subtree in
        match subtree with
          | Leaf l -> mark_leaf l tree idx static_root func
          | Node n -> begin
            mark_subtree n.node.a tree idx static_root func ;
            mark_subtree n.node.b tree idx static_root func
          end

  let leaf_make tree obj bb =
    let open Subtree in
        Leaf { bb ; parent = None ; node = { obj ; stamp = 0 ; pairs = None } }

  let leaf_update leaf tree index =
    let open Subtree in
        let bb = index.bb_func leaf.node.obj in
        if not (CpBB.contains_bb leaf.bb bb)
        then begin
          leaf.bb <- get_bb tree index leaf.node.obj ;
          let root = subtree_remove tree.root (Some leaf) tree in
          tree.root <- Some (subtree_insert root leaf tree) ;
          pairs_clear leaf tree ;
          leaf.node.stamp <- get_stamp tree index ;
          true
        end
        else false

  let leaf_add_pairs leaf tree idx =
    let dynamic_index = idx.dynamic_index in
    match dynamic_index with
      | Some _ -> begin
        let dynamic_root = get_root_if_tree dynamic_index in
        match dynamic_root with
          | Some dyn_root ->
              let dyn_tree = get_tree dynamic_index in
              mark_leaf_query dyn_root leaf true dyn_tree (fun _ _ -> ())
          | None -> ()
      end
      | None -> 
          let static_root = get_root_if_tree idx.static_index in
          mark_leaf leaf tree idx static_root (fun _ _ -> ())

  module LeafSet =
  struct
    let eql obj leaf =
      obj == Subtree.(leaf.node.obj)

    let trans obj (tree,idx) =
      let open Subtree in
          match leaf_make tree obj (idx.bb_func obj) with
            | Leaf leaf ->  leaf
            | _ -> assert false
                
  end

  let make () =
    { 
      velocity_func = None ;
      leaves = CpHashSet.make 0 LeafSet.eql ;
      root = None ;
      stamp = 0 
    }
      
  let set_velocity_func bb_tree func =
    bb_tree.velocity_func <- Some func

  let insert tree idx obj hashid =
    let open Subtree in
        let leaf = CpHashSet.insert tree.leaves hashid obj LeafSet.trans (tree,idx) in
        let root = tree.root in
        tree.root <- Some (subtree_insert root leaf tree) ;
        leaf.node.stamp <- get_stamp tree idx;
        leaf_add_pairs leaf tree idx;
        increment_stamp tree idx

  let remove tree idx obj hashid = 
    let open Subtree in
        let leaf = CpHashSet.remove tree.leaves hashid obj in
        tree.root <- subtree_remove tree.root leaf tree;
        pairs_clear CpOption.(!?leaf) tree

  let contains tree idx obj hashid =
    CpHashSet.find tree.leaves hashid obj <> None

  let reindex_query tree idx func =
    if tree.root <> None
    then begin
      CpHashSet.each tree.leaves (fun l -> ignore (leaf_update l tree idx)) ;

      let static_index = idx.static_index in
      let static_root = 
        match static_index with
          | Some { strategy = BBTree t ; _ } -> t.root
          | _ -> None
      in
      mark_subtree CpOption.(!?(tree.root)) tree idx static_root func ;
      (match (static_index, static_root) with
        | (Some static_idx, None) -> SIdx.collide_static idx static_idx func
        | _ -> () );      
      increment_stamp tree idx
    end

  let reindex tree idx =
    reindex_query tree idx (fun _ _ -> ())

  let reindex_object tree idx obj hashid =
    let leaf = CpHashSet.find tree.leaves hashid obj in
    match leaf with
      | Some l -> begin
        if leaf_update l tree idx then leaf_add_pairs l tree idx ;
        increment_stamp tree idx
      end 
      | None -> ()


  let segment_query tree idx obj a b t_exit func =
    match tree.root with
      | Some r -> ignore (subtree_segment_query r obj a b t_exit func)
      | None -> ()

  let query tree idx obj bb func =
    match tree.root with
      | Some r -> subtree_query r obj bb func
      | None -> ()

  let count tree idx =
    CpHashSet.count tree.leaves

  let each tree idx func =
    CpHashSet.each tree.leaves (fun l -> func Subtree.(l.node.obj))

  let rec partition_nodes tree nodes offset count =
    let open Subtree in
        if count == 1 then Leaf nodes.(offset)
        else if count == 2 then node_make tree (Leaf nodes.(offset)) (Leaf nodes.(1 + offset))
        else begin
          let bb = begin
            let bb_tmp = ref (CpBB.make 0. 0. 0. 0.) in
            for i = 0 to count - 1 
            do
              bb_tmp := CpBB.merge !bb_tmp nodes.(i + offset).bb
            done ;
            !bb_tmp
          end
          in
          let split_width = CpBB.(bb.r -. bb.l > bb.t -. bb.b) in

          let bounds = Array.init (count * 2) (fun i -> 
            match (i mod 2, split_width) with
              | (0, true) -> CpBB.(nodes.(i/2  + offset).bb.l)
              | (1, true) -> CpBB.(nodes.(i/2  + offset).bb.r)
              | (0, false) -> CpBB.(nodes.(i/2 + offset).bb.b)
              | (1, false) -> CpBB.(nodes.(i/2 + offset).bb.t)
              | _ -> assert false
          ) in
          Array.sort compare bounds ;
          let split = (bounds.(count - 1) +. bounds.(count)) *. 0.5 in
          let (a,b) = CpBB.(
            if split_width
            then (make bb.l bb.b split bb.t, make split bb.b bb.r bb.t)
            else (make bb.l bb.b bb.r split, make bb.l split bb.r bb.t)
          ) in

          let right = ref count in
          let left = ref 0 in
          while !left < !right
          do
            let node = nodes.(!left + offset) in
            if (CpBB.merged_area node.bb b) < (CpBB.merged_area node.bb a)
            then begin
              decr right;
              nodes.(!left + offset) <- nodes.(!right + offset) ;
              nodes.(!right + offset) <- node
            end 
            else incr left
          done ;

          if !right = count
          then begin
            let node = ref None in
            for i = 0 to count - 1
            do
              node := Some (subtree_insert !node nodes.(i + offset) tree)
            done ; CpOption.(!? !node)
          end
          else node_make tree 
            (partition_nodes tree nodes offset count) 
            (partition_nodes tree nodes (offset + !right) (count - !right))
        end 

  let optimize tree idx =
    let root = tree.root in
    match root with
        Some r -> begin
          let count = count tree idx in
          let dummy_node = Subtree.(
            { bb = CpBB.make 0. 0. 0. 0. ;
              parent = None ;
              node = { obj = None ; pairs = None ; stamp = 0 }
            } 
          ) in
          let nodes = Array.make count dummy_node in
          let i = ref 0 in
          CpHashSet.each tree.leaves (fun leaf -> (nodes.(!i) <- leaf ; incr i)) ;
          tree.root <- Some (partition_nodes tree nodes 0 count)
        end
      | None -> ()
end
