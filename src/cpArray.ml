type 'a t =
    {
      arr:'a array ;
      offset:int ;
      length:int ;
    }

let get a idx =
  a.arr.(idx + a.offset)

let set a idx v =
  a.arr.(idx + a.offset) <- v

let make arr offset length =
  assert (offset + length <= Array.length arr) ;
  { arr ; offset ; length }
    
let subrange a offset length =
  make a.arr (a.offset + offset) length

let length a =
  a.length

let copy a =
  { arr = Array.init (length a) (fun i -> (get a i) ) ; offset = 0 ; length = a.length}

let memcpy dest src =
  Array.blit src.arr src.offset dest.arr dest.offset (length src)

let iter func a =
  let sz = length a in
  let rec impl i = if i < sz then ( func (get a i) ; impl (i+1) )
  in impl 0

let iteri func a =
  let sz = length a in
  let rec impl i = if i < sz then ( func i (get a i) ; impl (i+1) )
  in impl 0

let fold_left func init_val a =
  let sz = length a in
  let rec impl v idx =
    if idx < sz
    then impl (func v (get a idx)) (idx+1)
    else v
  in
  impl init_val 0

let modify func a =
  let sz = length a in
  let rec impl i = if i < sz then (set a i (func (get a i)) ; impl (i+1) )
  in impl 0
  
