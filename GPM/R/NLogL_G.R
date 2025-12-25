NLogL_G <-  function(Omega, X, Y, CorrType, MinEig, Fn, n, dy){

  ALL <-  Auxil(Omega, X, Y, CorrType, MinEig, Fn, n, dy)
  R <- ALL$R
  L <- ALL$L
  Rinv_Y_FnB <- ALL$Rinv_Y_FnB
  Y_FnB <- ALL$Y_FnB
  Sigma2 <- ALL$Sigma2
  
  dOmega = length(Omega)
  nlogl_g <- rep(0, dOmega)
  if (CorrType == 'PE'){
    Power <- Omega[dOmega]
    temp <- matrix(0, n, n)
    for(i in 1: n){
      k <- abs(X - pracma::repmat(X[i, ], n, 1))
      k <- (k^Power)*log(k)
      k[is.nan(k)] <- 0
      temp[, i] <- rowSums(k*pracma::repmat(10^(Omega[-dOmega]), n, 1))
    }
    
    dR_dOmega <- -temp*R
    d_det_Sigma2 <- -1/n*t(Y_FnB)%*%CppSolve(t(L), CppSolve(L, dR_dOmega%*%Rinv_Y_FnB))
    nlogl_g[dOmega] <- dy*pracma::Trace(CppSolve(t(L), CppSolve(L, dR_dOmega))) + 
      n*pracma::Trace(CppSolve(Sigma2, d_det_Sigma2))
  }else{
    Power <- 2
  }
  for (i in 1: (dOmega - as.numeric(CorrType == 'PE'))){
    temp <- pracma::distmat(X[, i, drop = FALSE], X[, i, drop = FALSE])
    dR_dOmega <- -10^Omega[i]*log(10)*(temp^Power)*R
    d_det_Sigma2 <- -1/n*t(Y_FnB)%*%CppSolve(t(L), CppSolve(L, dR_dOmega%*%Rinv_Y_FnB))
    nlogl_g[i] <- dy*pracma::Trace(CppSolve(t(L), CppSolve(L, dR_dOmega))) + 
      n*pracma::Trace(CppSolve(Sigma2, d_det_Sigma2))
  }

  return(nlogl_g)
}