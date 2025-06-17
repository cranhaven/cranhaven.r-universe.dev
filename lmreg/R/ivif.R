ivif <- 
  function(lmobj) {
    vif <- diag(vcov(lmobj))*diag(solve(vcov(lmobj)))
    return(vif)
  }
