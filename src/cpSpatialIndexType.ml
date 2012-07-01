type 'a bb_func = 'a -> CpBB.t
type 'a iterator_func = 'a -> unit
type ('a,'b) query_func = 'b -> 'a -> unit
type ('a,'b) segment_query_func = 'b -> 'a -> float

module BBTreeImpl =
struct
  type 'a velocity_func = 'a -> CpVector.t

  module Subtree =
  struct 
    type ('a,'b) subtree_struct =
        {
          mutable parent:'a node option;
          mutable bb:CpBB.t ;
          mutable node:'b
        }
    and 'a node_data =
        {
          mutable a:'a subtree ;
          mutable b:'a subtree
        } 
    and 'a leaf_data =
        {
          mutable obj:'a ;         
          mutable stamp:int ;
          mutable pairs:'a pair option
        }
    and 'a node = ('a, 'a node_data) subtree_struct
    and 'a leaf = ('a, 'a leaf_data) subtree_struct
    and 'a subtree =
      | Node of 'a node  
      | Leaf of 'a leaf
    and 'a thread =
        {
          mutable prev:'a pair option ;
          mutable leaf:'a leaf ;
          mutable next:'a pair option
        }
    and 'a pair =
      {
        thread_a:'a thread ;
        thread_b:'a thread
      }
  end  
  type 'a t =
      {
        mutable velocity_func:'a velocity_func option ;
        leaves:('a, 'a Subtree.leaf) CpHashSet.t;
        mutable root:'a Subtree.subtree option;
        mutable stamp:int
      }
end

module SpaceHashImpl =
struct
  module Handle =
  struct
    type 'a t =
        {
          mutable obj:'a option ;
          mutable stamp:int
        }

    let obj hand =
      match hand.obj with
        | Some o -> o
        | None -> failwith "Orphaned handle dereferenced"  

  end

  type 'a t =
      {      
        mutable celldim:float ;
        mutable table:'a Handle.t list array ;
        handle_set:('a, 'a Handle.t) CpHashSet.t ;
        mutable stamp:int ;
      }
end


type 'a strategy_type = 
    BBTree of 'a BBTreeImpl.t
  | SpaceHash of 'a SpaceHashImpl.t
      
type 'a t = 
    {
      strategy:'a strategy_type ;
      bb_func:'a bb_func ;
      static_index:'a t option ;
      mutable dynamic_index:'a t option
    }
