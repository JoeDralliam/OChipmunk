open CpSpatialIndexType

module Make 
  ( SIdx : 
    sig 
      val collide_static : 'a t -> 'a t -> ('a,'a) query_func -> unit 
    end
  ) =
struct
  open SpaceHashImpl
  let do_if f = function | Some obj -> f obj | None -> ()
    
  module HandleSet =
  struct
    open Handle
      
    let eql obj hand =
      CpOption.(hand.obj ==? obj)
        
    let trans obj _ =
      { obj = Some obj ; stamp = 0 }
  end

  let clear_table_cell hash i =
    hash.table.(i) <- []

  let clear_table hash =
    for i = 0 to (Array.length hash.table)-1 do clear_table_cell hash i done


  let make celldim numcells =
    { celldim ;
      table = Array.make Prime.(next_prime numcells) [] ;
      handle_set = CpHashSet.make 0 HandleSet.eql ;
      stamp = 1 }


  let contains_handle list handle =
    List.exists (fun h -> h == handle) list


  let hash_func x y n = 
    let (x,y,n) = Int64.(abs (of_int x), abs (of_int y), of_int n) in
    Int64.(to_int (rem (logxor (mul x 1640531513L) (mul y 2654435789L)) n))



  let hash_handle hash hand bb =
    let dim = hash.celldim in
    let l = truncate (floor (CpBB.(bb.l)/.dim)) in
    let r = truncate (floor (CpBB.(bb.r)/.dim)) in
    let b = truncate (floor (CpBB.(bb.b)/.dim)) in
    let t = truncate (floor (CpBB.(bb.t)/.dim)) in

    let n = Array.length hash.table in
    for i=l to r
    do
      for j=b to t
      do
        let idx = hash_func i j n in
        let list = hash.table.(idx) in

        if not (contains_handle list hand)
        then hash.table.(idx) <- hand :: hash.table.(idx)
      done
    done 

  let insert hash spatial_index obj hashid =
    let hand = CpHashSet.insert hash.handle_set hashid obj HandleSet.trans hash in
    hash_handle hash hand (spatial_index.bb_func obj)

  let rehash_object hash spatial_index obj hashid =
    let hand = CpHashSet.remove hash.handle_set hashid obj in
    do_if (fun h -> begin
      Handle.(h.obj <- None) ;
      insert hash spatial_index obj hashid
    end) hand

  let reindex_object = rehash_object

  let rehash hash spatial_index =
    clear_table hash;
    CpHashSet.each hash.handle_set 
      (fun hand -> hash_handle hash hand (spatial_index.bb_func Handle.(obj hand)) )

  let reindex = rehash

  let remove hash _ obj hashid =
    let hand = CpHashSet.remove hash.handle_set hashid obj in
    do_if (fun h -> Handle.(h.obj <- None)) hand
      
      
  let each hash _ func =
    CpHashSet.each hash.handle_set (fun hand -> func Handle.(obj hand) )

  let remove_orphaned_handle hash idx =
    let list = hash.table.(idx) in
    hash.table.(idx) <- List.filter (fun h -> Handle.(h.obj <> None)) list

  let rec query_helper hash idx obj func =
    while not (List.for_all (
      fun hand ->
        let other = Handle.(hand.obj) in
        if Handle.(hand.stamp) = hash.stamp  (* || CpOption.(other ==? obj) ??? *)
        then true
        else if other <> None
        then begin
          func obj Handle.(obj hand) ; 
          let stamp = hash.stamp in
          Handle.(hand.stamp <- stamp) ; 
          true
        end
        else ( remove_orphaned_handle hash idx ; false )
    ) hash.table.(idx))
    do ()
    done

  let query hash spatial_index obj bb func =
    let dim = hash.celldim in
    let l = truncate (floor (CpBB.(bb.l)/.dim)) in
    let r = truncate (floor (CpBB.(bb.r)/.dim)) in
    let b = truncate (floor (CpBB.(bb.b)/.dim)) in
    let t = truncate (floor (CpBB.(bb.t)/.dim)) in

    let n = Array.length hash.table in
    for i = l to r
    do
      for j = b to t
      do
        query_helper hash (hash_func i j n) obj func
      done
    done ;
    hash.stamp <- hash.stamp + 1

  let reindex_query hash spatial_index func =
    clear_table hash;
    CpHashSet.each hash.handle_set (fun hand -> 
      let dim = hash.celldim in
      let n = Array.length hash.table in
      let obj = Handle.obj hand in
      let bb = (spatial_index.bb_func obj) in

      let l = truncate (floor (CpBB.(bb.l)/.dim)) in
      let r = truncate (floor (CpBB.(bb.r)/.dim)) in
      let b = truncate (floor (CpBB.(bb.b)/.dim)) in
      let t = truncate (floor (CpBB.(bb.t)/.dim)) in

      for i = l to r
      do
        for j = b to t
        do
          let idx = hash_func i j n in
          let list = hash.table.(idx) in
          if not (contains_handle list hand)
          then begin
            query_helper hash idx obj func ;
            hash.table.(idx) <- hand :: list
          end
        done
      done ;
      hash.stamp <- hash.stamp + 1
    ) ;
    SIdx.collide_static spatial_index CpOption.(!? (spatial_index.static_index)) func
      


  let rec segment_query_helper hash idx obj func =
    let t = ref 1. in
    while not (List.for_all (
      fun hand ->
        let other = Handle.(hand.obj) in
        if Handle.(hand.stamp) <> hash.stamp
        then true 
        else if other <> None
        then begin 
          t := min !t (func obj Handle.(obj hand)) ;
          let stamp = hash.stamp in
          Handle.(hand.stamp <- stamp) ; 
          true
        end
        else ( remove_orphaned_handle hash idx ; false )
    ) hash.table.(idx))
    do ()
    done ;
    !t

  let segment_query hash spatial_index obj a b t_exit func =
    let open CpVector in
        let celldim = hash.celldim in
        let a = mult a (1. /. celldim) in
        let b = mult b (1. /. celldim) in
        
        let t_exit = ref t_exit in
        
        let cell_x = ref (truncate (floor (a.x))) in
        let cell_y = ref (truncate (floor (a.y))) in

        let t = ref 0. in

        let (x_inc, temp_h) =
          if b.x > a.x
          then (1 , floor (a.x +. 1.) -. a.x)
          else (-1, a.x -. floor a.x)
        in
        let (y_inc, temp_v) =
          if b.y > a.y
          then (1 , floor (a.x +. 1.) -. a.x)
          else (-1, a.y -. floor a.y)
        in

        let dx = abs_float (b.x -. a.x) in
        let dy = abs_float (b.y -. a.y) in
        let dt_dx = if dx > 0. then 1. /. dx else infinity in
        let dt_dy = if dy > 0. then 1. /. dy else infinity in

        let next_h = ref (if temp_h <> 0. then temp_h *. dt_dx else dt_dx) in
        let next_v = ref (if temp_v <> 0. then temp_v *. dt_dy else dt_dy) in

        let n = Array.length hash.table in
        while !t < !t_exit
        do 
          let idx = hash_func !cell_x !cell_y n in
          t_exit := min !t_exit (segment_query_helper hash idx obj func) ;
          if !next_v < !next_h
          then (cell_y := !cell_y + y_inc ; t := !next_v ; next_v := dt_dy) 
          else (cell_x := !cell_x + x_inc ; t := !next_h ; next_h := dt_dx) 
        done ;
        hash.stamp <- hash.stamp + 1

  let resize hash _ celldim numcells =
    clear_table hash;
    hash.celldim <- celldim ;
    hash.table <- Array.make (Prime.next_prime numcells) []

  let count hash _ =
    CpHashSet.count hash.handle_set

  let contains hash _ obj hashid =
    (CpHashSet.find hash.handle_set hashid obj) <> None
end


(*

  class ['a] space_hash celldim' numcells bb_func static_index =
  object (self)
  inherit CpSpatialIndex.strategy
  
  val mutable celldim = celldim'
  val mutable table = Array.make numcells []
  val handle_set = CpHashSet.make 0 Handle.eql
  val mutable stamp = 1

  end

*)
