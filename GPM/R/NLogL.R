NLogL <-  function(Omega, X, Y, CorrType, MinEig, Fn, n, dy){
  
  ALL <-  Auxil(Omega, X, Y, CorrType, MinEig, Fn, n, dy)
  R <- ALL$R
  L <- ALL$L
  
  if (CorrType == 'PE' || CorrType=='G'){
    Sigma2 <- ALL$Sigma2
    nlogl = n*log(det(Sigma2)) + dy*2*sum(log(diag(L)))
  }else{
    Alpha <- ALL$Alpha
    nlogl = n*log(det(Alpha)) + dy*2*sum(log(diag(L)))
  }
  
  return(nlogl)
}
