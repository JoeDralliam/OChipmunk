open CpSpatialIndexType

type 'a t = 'a CpSpatialIndexType.t

let dispatch index bb_tree_func space_hash_func =
  match index.strategy with
      BBTree tree -> bb_tree_func tree index
    | SpaceHash hash -> space_hash_func hash index

let make strategy bb_func static_index =
  let index = { strategy ; bb_func ; static_index ; dynamic_index = None } in
  (match static_index with
    | Some idx -> idx.dynamic_index <- Some index
    | None -> () ) ;
  index



module rec Impl : sig
  val count : 'a t -> int
  val each : 'a t -> ('a iterator_func) -> unit

  val contains : 'a t -> 'a -> CpHashSet.hash_type -> bool
  val insert : 'a t -> 'a -> CpHashSet.hash_type -> unit
  val remove : 'a t -> 'a -> CpHashSet.hash_type -> unit

  val reindex : 'a t -> unit
  val reindex_object : 'a t -> 'a -> CpHashSet.hash_type -> unit
  val reindex_query : 'a t -> ('a,'a) query_func -> unit

  val query : 'a t -> 'b -> CpBB.t -> ('a,'b) query_func -> unit
  val segment_query : 'a t -> 'b -> CpVector.t -> CpVector.t -> float -> ('a,'b) segment_query_func -> unit

  val collide_static : 'a t -> 'a t -> ('a,'a) query_func -> unit

  val make_bb_tree : ('a bb_func) -> 'a t option -> 'a t

  val make_space_hash : float -> int -> ('a bb_func) -> 'a t option -> 'a t
end =
struct      
  module BBTree = CpBBTree.Make (Impl)
  module SpaceHash = CpSpaceHash.Make (Impl)


  let count index = 
    dispatch index 
      (fun t idx -> BBTree.count t idx)
      (fun h idx -> SpaceHash.count h idx)
      
  let each index func =
    dispatch index 
      (fun t idx -> BBTree.each t idx func)
      (fun h idx -> SpaceHash.each h idx func)

  let query index obj bb func =
    dispatch index 
      (fun t idx -> BBTree.query t idx obj bb func)
      (fun h idx -> SpaceHash.query h idx obj bb func)
      

  let collide_static dynamic_index static_index func =
    if count static_index > 0
    then
      each dynamic_index (fun obj -> query static_index obj (dynamic_index.bb_func obj) func)

  let contains index obj hashid = 
    dispatch index 
      (fun t idx -> BBTree.contains t idx obj hashid)
      (fun h idx -> SpaceHash.contains h idx obj hashid)
      
  let insert index obj hashid = 
    dispatch index 
      (fun t idx -> BBTree.insert t idx obj hashid)
      (fun h idx -> SpaceHash.insert h idx obj hashid)

  let remove index obj hashid =
    dispatch index 
      (fun t idx -> BBTree.remove t idx obj hashid)
      (fun h idx -> SpaceHash.remove h idx obj hashid)
      
  let reindex index =
    dispatch index 
      (fun t idx -> BBTree.reindex t idx)
      (fun h idx -> SpaceHash.reindex h idx)
      
  let reindex_object index obj hashid =
    dispatch index 
      (fun t idx -> BBTree.reindex_object t idx obj hashid)
      (fun h idx -> SpaceHash.reindex_object h idx obj hashid)
      
  let segment_query index obj a b t_exit func =
    dispatch index 
      (fun t idx -> BBTree.segment_query t idx obj a b t_exit func)
      (fun h idx -> SpaceHash.segment_query h idx obj a b t_exit func)
      
  let reindex_query index func =
    dispatch index 
      (fun t idx -> BBTree.reindex_query t idx func)
      (fun h idx -> SpaceHash.reindex_query h idx func)

  let make_bb_tree bb_func static_index =
    let tree = BBTree.make () in
    make (BBTree tree) bb_func static_index
      
  let make_space_hash celldim cells bb_func static_index =
    let space_hash = SpaceHash.make celldim cells in
    make (SpaceHash space_hash) bb_func static_index
end

module BBTree = CpBBTree.Make (Impl)
module SpaceHash = CpSpaceHash.Make (Impl)

let make_bb_tree = Impl.make_bb_tree
let make_space_hash = Impl.make_space_hash

let count = Impl.count
let each = Impl.each

let contains = Impl.contains
let insert = Impl.insert
let remove = Impl.remove

let reindex = Impl.reindex
let reindex_object = Impl.reindex_object
let reindex_query = Impl.reindex_query

let query = Impl.query
let segment_query = Impl.segment_query

let collide_static = Impl.collide_static
