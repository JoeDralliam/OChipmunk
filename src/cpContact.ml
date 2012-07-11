type t =
    {
      p:CpVector.t ;
      n:CpVector.t ;
      dist:float ;

      r1:CpVector.t ;
      r2:CpVector.t ;
      n_mass:float ;
      t_mass:float ;
      bounce:float ;

      jn_acc:float ;
      jt_acc:float ;
      j_bias:float ;
      bias:float ;

      hash:int64
    }

let init con p n dist hash =
  { 
    con with 
      p ; n ; dist ;
      jn_acc = 0. ; jt_acc = 0. ; j_bias = 0. ; 
      hash 
  }

let make () =
  { 
    p = CpVector.zero ;
    n = CpVector.zero ;
    dist = 0. ;
    
    r1 = CpVector.zero ;
    r2 = CpVector.zero ;
    n_mass = 0. ;
    t_mass = 0. ;
    bounce = 0. ;

    jn_acc = 0. ;
    jt_acc = 0. ;
    j_bias = 0. ;
    bias = 0. ;

    hash = 0L
  }
