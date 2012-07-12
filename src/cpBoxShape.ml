module Make = functor (Param : CpType.UserData) ->
struct
  module PolyShape = CpPolyShape.Make(Param)

  let make2 box =
    let verts = CpBB.( 
      [| CpVector.(make box.l box.b) ; 
         CpVector.(make box.l box.t) ;
         CpVector.(make box.r box.t) ;
         CpVector.(make box.r box.b)
      |] ) in

    PolyShape.make verts CpVector.zero

  let make width height =
    let hw = width  /. 2. in
    let hh = height /. 2. in

    make2 (CpBB.make (-.hw) (-.hh) hw hh)
end
