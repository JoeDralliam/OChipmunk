type hash_type = int64

module HashSetBin =
struct
  type 'b t =
      {
	mutable elt:'b ;
	mutable hash:hash_type
      }
end

type 'b hash_set_bins = 'b HashSetBin.t list

type ('a,'b) eql_func = 'a -> 'b -> bool

type ('a,'b) t =
    {
      mutable entries:int ;
      mutable size:int64 ;
      eql:('a,'b) eql_func ;
      mutable default_value:'b option ;
      mutable table:'b hash_set_bins array ;
    }

let make sz eql =
  let sizei = Prime.next_prime sz in
  let size = Int64.of_int sizei in
  { 
    size ; 
    entries = 0 ;
    eql ;
    default_value = None ;
    table =  Array.make sizei []
  }

let set_default_value set default_value =
  set.default_value <- default_value

let is_full set =
  set.entries >= (Int64.to_int set.size)

let resize set =
  let newSizei = Prime.next_prime Int64.(to_int (add set.size one)) in
  let newSize = Int64.of_int newSizei in
  let newTable = Array.make newSizei [] in
  let rec rehash_bin b =
    match b with
      | bin :: next -> HashSetBin.(
	let idx = Int64.(to_int (rem bin.hash newSize)) in
	newTable.(idx) <- (bin :: newTable.(idx)) ;
	rehash_bin next
      )
      | [] -> ()
  in
  
  Array.iter rehash_bin set.table ;
  set.table <- newTable ;
  set.size <- newSize

            
let count set = set.entries
  
let insert set hash ptr trans data =
  let idx = Int64.(to_int (rem hash set.size)) in
  let list = set.table.(idx) in
  let bin =
    try List.find (fun bin -> set.eql ptr HashSetBin.(bin.elt)) list
    with Not_found -> begin
      let bin = 
	HashSetBin.({ 
	  hash ; 
          elt = (trans ptr data)
        }) in
      set.table.(idx) <- bin :: set.table.(idx) ;
      set.entries <- set.entries + 1 ;
      if is_full set then resize set ;
      bin
    end
  in
  HashSetBin.(bin.elt)


let remove set hash ptr =
  let idx = Int64.(to_int (rem hash set.size)) in
  let list = set.table.(idx) in
  let (to_remove, to_keep) =
    List.partition (fun bin -> set.eql ptr HashSetBin.(bin.elt)) list in
  set.table.(idx) <- to_keep ;
  assert ((List.length to_remove) <= 1) ;
  if to_remove = [] then None else Some HashSetBin.((List.hd to_remove).elt)


let find set hash ptr =
  let idx = Int64.(to_int (rem hash set.size)) in
  let list = set.table.(idx) in
  try begin
    let bin = List.find (fun bin -> set.eql ptr HashSetBin.(bin.elt)) list in
    Some HashSetBin.(bin.elt)
  end 
  with Not_found -> set.default_value

let each set func =
  Array.iter (List.iter (fun bin -> func HashSetBin.(bin.elt)) ) set.table

let filter set func =
  for i = 0 to (Int64.to_int set.size) - 1
  do
    let list = set.table.(i) in
    set.table.(i) <- List.filter (fun bin -> begin
      let keep = func HashSetBin.(bin.elt) in
      if not keep then set.entries <- set.entries - 1 ;
      keep
    end) list
  done 

(*



  module type HashedType =
  struct
  type key
  type t
  val eql : key -> t -> bool
  val trans : key -> t
  end

  module Make =
  functor (H:HashedType) ->
  struct
  module Hashed =
  struct
  type t = (H.key) ;
  let eql a b = H.eql a (H.trans b) 
  let hash = Hashtbl.hash
  end

  module Hashtable = Hashtbl.Make Hashed
  
  type t =
  {
  table:H.t Hashtable.t
  mutable default_value:H.t option
  }
  
  let set_default_value set default_value =
  set.default_value <- default_value
  
  let count set = Hashtable.length set.table

  let insert set key =
  Hashtable.find set.table key (H.trans key)
  
  let remove set key =
  Hashtable.remove set.table key

  let find set key =
  try Some (Hashtable.find set.table key)
  with set.default_value
  end


*)
