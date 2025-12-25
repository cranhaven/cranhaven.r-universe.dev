Auxil <-  function(Omega, X, Y, CorrType, MinEig, Fn, n, dy){
  
  R <- CorrMat_Sym(X, CorrType, Omega)
  
  Raw_MinEig = Eigen(R)
  if (Raw_MinEig < MinEig){
    R = R + diag(x = 1, n, n)*(MinEig - Raw_MinEig)
  }
  L = LowerChol(R)
  
  ALL <- list('R' = R, 'L' = L, 'Raw_MinEig' = Raw_MinEig, 'Nug_opt' = max(0, MinEig - Raw_MinEig))
  
  if (CorrType == 'PE' || CorrType=='G'){
    RinvFn = CppSolve(t(L), CppSolve(L, Fn))
    FnTRinvFn = t(Fn)%*%RinvFn
    RinvY <- CppSolve(t(L), CppSolve(L, Y))
    B = t(Fn)%*%RinvY/FnTRinvFn[1]
    Rinv_Y_FnB = RinvY - RinvFn%*%B
    Y_FnB <- Y - Fn%*%B
    Sigma2 = t(Y_FnB)%*%Rinv_Y_FnB/n
    
    ALL$B <- B
    ALL$Rinv_Y_FnB <- Rinv_Y_FnB
    ALL$Y_FnB <- Y_FnB
    ALL$Sigma2 <- Sigma2
  }else{
    Alpha = t(Y)%*%CppSolve(t(L), CppSolve(L, Y))/n
    
    ALL$Alpha <- Alpha
  }
  
  return(ALL)
}
