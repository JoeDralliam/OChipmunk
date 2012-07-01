let make2 box =

  let verts = CpBB.( 
    [| CpVector.(make box.l box.b) ; 
       CpVector.(make box.l box.t) ;
       CpVector.(make box.r box.t) ;
       CpVector.(make box.r box.b)
    |] ) in

  CpPolyShape.make verts CpVector.zero

let make width height =
  let hw = width  /. 2. in
  let hh = height /. 2. in

  make2 (CpBB.make (-.hw) (-.hh) hw hh)
