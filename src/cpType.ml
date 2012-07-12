module type UserData =
sig
  type body_data
  type space_data
  type shape_data
  type constraint_data
end

module Make = functor (Param : UserData) ->
struct
  module ConstraintClass =
  struct
    class virtual ['a] t =
    object
      method virtual prestep : 'a -> float -> unit
      method virtual apply_cached_impulse : 'a -> float -> unit
      method virtual apply_impulse : 'a -> unit
      method virtual get_impulse : 'a -> float
    end
  end


  module ContactBufferType =
  struct
    type t =
        {
          mutable stamp:int ;
          mutable next:t;
          mutable num_contacts:int ;
          contacts:CpContact.t array
        }
  end

  module ComponentNode =
  struct
    type 'a t = 
        { 
	  mutable root:('a option) ;
	  mutable next:('a option) ;
	  mutable idle_time:float
        }
  end

  module ContactPointSet =
  struct
    type u = { point:CpVector.t ; normal:CpVector.t ; dist:float }
    type t = { count:int ; points:u array }
  end
    
  type timestamp' = int
  type arbiter_state' = FirstColl | Normal | Ignore | Cached
      
  let max_contacts_per_arbiter = 4 (* cpArbiter.h *)



  type 'a begin_func    = 'a -> space' -> bool
  and 'a presolve_func  = 'a -> space' -> bool
  and 'a postsolve_func = 'a -> space' -> unit
  and 'a separate_func  = 'a -> space' -> unit
  and collision_handler' =
      {
        cha:int ;
        chb:int ;
        chbegin:arbiter' begin_func ;
        chpresolve:arbiter' presolve_func ;
        chpostsolve:arbiter' postsolve_func ;
        chseparate:arbiter' separate_func ;
      }
  and body' = 
      {
        mutable bvelocity_func:(body' -> CpVector.t -> float -> float -> unit) ;
        mutable bposition_func:(body' -> float -> unit) ;
        
        mutable bm:float ;
        mutable bm_inv:float ;
        mutable bi:float ;
        mutable bi_inv:float ;
        
        mutable bp:CpVector.t ;
        mutable bv:CpVector.t ;
        mutable bf:CpVector.t ;
        
        mutable ba:float ;
        mutable bw:float ;
        mutable bt:float ;
        
        mutable brot:CpVector.t ;
        
        
        mutable bv_limit:float ;
        mutable bw_limit:float ;
        
        mutable bv_bias:CpVector.t ;
        mutable bw_bias:float ;
        
        mutable bspace:space' option ;

        mutable bshape_list:shape' option ;
        mutable barbiter_list:arbiter' option ;
        mutable bconstraint_list:constraint' option ;
        
        mutable bdata:Param.body_data option;

        bnode:body' ComponentNode.t ;
      }
  and shape' = 
      {
        mutable shstrategy:CpShapeType.t ;
        mutable shbody:body' ;
        mutable shbb:CpBB.t ;
        mutable shsensor:bool ;
        mutable she:float ;
        mutable shu:float ;
        mutable shsurface_v:CpVector.t ;
        
        mutable shcollision_type:int ;
        mutable shgroup:int ;
        mutable shlayers:int ;
        
        mutable shspace:space' option;
        
        mutable shnext:shape' option ;
        mutable shprev:shape' option ;
        
        mutable shdata:Param.shape_data option;

        shhashid:int
      }
  and arbiter' =
      {
        mutable ae:float ; 
        mutable au:float ;
        mutable asurface_vr:CpVector.t ;

        mutable aa:shape' ;
        mutable ab:shape' ;
        mutable abody_a:body' ;
        mutable abody_b:body' ;

        athread_a:arbiter' thread' ;
        athread_b:arbiter' thread' ;

        mutable acontacts:CpContact.t CpArray.t ;

        mutable astamp:timestamp' ;
        mutable ahandler:collision_handler' option;
        mutable aswapped_coll:bool  ;
        mutable astate:arbiter_state' ;
      }
  and 'a thread' =
      {
        mutable thprev:'a option ;
        mutable thnext:'a option
      }
  and constraint_class' = constraint' ConstraintClass.t
  and constraint' = 
      {
        cclass:constraint_class' ;

        ca:body';
        cb:body';

        mutable cspace:space' option ;

        mutable cnext_a:constraint' option ;
        mutable cnext_b:constraint' option ;

        mutable cmax_force:float ;
        mutable cerror_bias:float ;
        mutable cmax_bias:float ;

        mutable cpresolve:(constraint' -> space' -> unit) option;
        mutable cpostsolve:(constraint' -> space' -> unit) option ;

        mutable cdata:Param.constraint_data option
      }
  and space' =
      {
        mutable spiterations:int ;
        mutable spgravity:CpVector.t ;
        mutable spdamping:float ;
        mutable spidle_speed_threshold:float ;
        mutable spsleep_time_threshold:float ;
        mutable spcollision_slop:float ;
        mutable spcollision_bias:float ;
        mutable spcollision_persistence:int ;
        mutable spenable_contact_graph:bool  ;
        spstatic_body:body' ;

        (* private fields *)
        mutable spstamp:int ;
        mutable spcurr_dt:float ;
        mutable spbodies:body' list ;
        mutable sproused_bodies:body' list ;
        mutable spsleeping_components:body' list ;

        mutable spstatic_shapes:shape' CpSpatialIndex.t ;
        mutable spactive_shapes:shape' CpSpatialIndex.t ;

        mutable sparbiters:arbiter' list ;
        mutable spcontact_buffers_head:ContactBufferType.t option;
        mutable spcached_arbiters:(shape' * shape', arbiter') CpHashSet.t ;
        mutable spconstraints:constraint' list ;

        mutable splocked:int ;

        mutable spcollision_handlers:(int * int,collision_handler') CpHashSet.t;
        mutable spdefault_handler:collision_handler' ;
        
        mutable spskip_post_step:bool ;
        mutable sppost_step_callbacks:(int * (space' -> unit)) list;

        mutable spdata:Param.space_data option
      }
end
