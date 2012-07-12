module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)
  module Private = CpPrivate.Make(Param)
  module Body = CpBody.Make(Param)
  open Type

type t = constraint'

let eql = Private.Constraint.eql

let activate_bodies con =
  Body.activate con.ca ;
  Body.activate con.cb
    
let get_space con = con.cspace

let get_a con = con.ca
let get_b con = con.cb

let get_max_force con = con.cmax_force
let set_max_force con v =
  activate_bodies con ;
  con.cmax_force <- v

let get_error_bias con = con.cerror_bias
let set_error_bias con v =
  activate_bodies con ;
  con.cerror_bias <- v

let get_max_bias con = con.cmax_bias
let set_max_bias con v =
  activate_bodies con ;
  con.cmax_bias <- v

let get_presolve con = con.cpresolve
let set_presolve con v =
  activate_bodies con ;
  con.cpresolve <- v

let get_postsolve con = con.cpostsolve
let set_postsolve con v =
  activate_bodies con ;
  con.cpostsolve <- v

let get_impulse con =
  con.cclass#get_impulse con

let make cclass ca cb =
  {
    cclass ; 
    ca ; cb ; 
    cspace = None ;

    cnext_a = None ;
    cnext_b = None ;

    cmax_force = infinity ;
    cerror_bias = (1. -. 0.1) ** 60. ;
    cmax_bias = infinity ;

    cpresolve = None ;
    cpostsolve = None ;

    cdata = None
  }
    
end
