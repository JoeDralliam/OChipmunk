type t =
    {
      shape:CpType.shape' option ;
      t:float ;
      n:CpVector.t
    }

let hit_point start end' info =
  CpVector.lerp start end' info.t

let distance start end' info =
  (CpVector.dist start end') *. info.t
