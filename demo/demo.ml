open OChipmunkDemo

let _ =
  demos := [| 
    ((new Planet.planet) :> demo) ;
    ((new PyramidTopple.pyramid_topple) :> demo) ;
    ((new Tumble.tumble) :> demo)
           |] ;
  let app = init () in
  let info = run_demo 0 app in
  main_loop info
