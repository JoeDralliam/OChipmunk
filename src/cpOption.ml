let (==??) opta optb =
  match (opta,optb) with
    | (Some a, Some b) -> a == b
    | (None, None) -> true
    | _ -> false


let (=?) opt v = 
  match opt with
    | Some a -> a = v
    | None -> false

let (==?) opt v =
  match opt with
    | Some a -> a == v
    | None -> false

let (<>?) opt v =
  match opt with
    | Some a -> a <> v
    | None -> true

let (!=?) opt v =
  match opt with
    | Some a -> a != v
    | None -> true

let (!?) opt =
  match opt with
    | Some a -> a
    | None -> failwith "Trying to dereference None"

let do_if f opt =
  match opt with
    | Some a -> f a
    | None -> ()

let do_opt f opt =
  match opt with
    | Some a -> Some (f a)
    | None -> None


