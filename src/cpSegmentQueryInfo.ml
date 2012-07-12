module Make = functor (Param : CpType.UserData) ->
struct
  module Type = CpType.Make(Param)


  type t =
      {
        shape:Type.shape' option ;
        t:float ;
        n:CpVector.t
      }

  let hit_point start end' info =
    CpVector.lerp start end' info.t
      
  let distance start end' info =
    (CpVector.dist start end') *. info.t
end
