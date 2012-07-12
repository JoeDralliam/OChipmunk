module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)

  type t =
      {
        shape:Type.shape' option ;
        p:CpVector.t ;
        d:float
      }
end
