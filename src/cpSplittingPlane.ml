type t =
    {
      n:CpVector.t ;
      d:float
    }
      

let make a b =
  let open CpVector in
      let n = normalize (perp (sub b a)) in
      let d = dot n a in
      { n ; d }

let compare plane v =
  (CpVector.dot plane.n v) -. plane.d

