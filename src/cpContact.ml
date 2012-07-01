type t =
    {
      mutable p:CpVector.t ;
      mutable n:CpVector.t ;
      mutable dist:float ;

      mutable r1:CpVector.t ;
      mutable r2:CpVector.t ;
      mutable n_mass:float ;
      mutable t_mass:float ;
      mutable bounce:float ;

      mutable jn_acc:float ;
      mutable jt_acc:float ;
      mutable j_bias:float ;
      mutable bias:float ;

      mutable hash:int64
    }

let init con p n dist hash =
  con.p <- p ;
  con.n <- n ;
  con.dist <- dist ;

  con.jn_acc <- 0. ;
  con.jt_acc <- 0. ;
  con.j_bias <- 0. ;

  con.hash <- hash ;
  con

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
