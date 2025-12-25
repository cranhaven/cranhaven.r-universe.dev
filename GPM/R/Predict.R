Predict <-  function(XF, Model, MSE_on = 0, YgF_on = 0, grad_dim = rep(1, ncol(XF))){

  XN = Model$Data$XN
  dx = ncol(XN)
  MSE = NULL
  YgF = NULL
  if (ncol(XF) != dx){
    stop('The dimension of XF is not correct!')
  }
  if (class(Model) != "GPM"){
    stop('The 2nd input should be a model of class GPM built by GPM::Fit.')
  }
  if (length(MSE_on)!=1 || length(YgF_on)!=1){
    stop('MSE_on and YgF_on should be scalar numerics. Non-zero values will turn them "on".')
  }
  CorrType = Model$CovFun$CorrType
  Ymin = Model$Data$Ymin
  Yrange = Model$Data$Yrange
  n = Model$Data$n
  Xmin = Model$Data$Xmin
  Xmax = Model$Data$Xmax
  dy = Model$Data$dy
  Fn = Model$Details$Fn
  L = Model$Details$L
  Nug_opt <- Model$Details$Nug_opt

  m = nrow(XF)
  Fm = matrix(1, m, 1)
  XFN = t((t(XF)-Xmin)/(Xmax-Xmin))

  if (CorrType == 'PE' || CorrType=='G'){
    Theta = Model$CovFun$Parameters$Theta
    Power = Model$CovFun$Parameters$Power
    B = Model$CovFun$Parameters$B
    Rinv_YN = Model$CovFun$Parameters$Rinv_YN
    RinvFn = Model$CovFun$Parameters$RinvFn
    FnTRinvFn = Model$CovFun$Parameters$FnTRinvFn
    Sigma2 = Model$CovFun$Parameters$Sigma2
    if (CorrType == 'PE'){
      Rxf = CorrMat_Vec(XN, XFN, CorrType, c(Theta, Power))
    } else {
      Rxf = CorrMat_Vec(XN, XFN, CorrType, Theta)
    }
    YFN = Fm%*%B + t(Rxf)%*%(Rinv_YN - RinvFn%*%B)
    YF = t(t(YFN)*Yrange + Ymin)
    if (MSE_on){
      Rinv_Rxf = CppSolve(t(L), CppSolve(L, Rxf))
      FnTRinv_Rxf = t(Fn)%*%Rinv_Rxf
      MSE <- 1 - matrix(colSums(Rxf*Rinv_Rxf), m, 1) + t(t(Fm) - FnTRinv_Rxf)^2 + Nug_opt
      MSE <- matrix(kronecker(matrix(diag(Sigma2)*Yrange^2, dy, 1), MSE), m, dy)
    }
    if (YgF_on){
      if (Power != 2){
        stop('The gradient can be calculated only if Power == 2.')
      }
      if (!is.vector(grad_dim)) grad_dim = as.vector(grad_dim)
      if (length(grad_dim) != dx) {
        stop(paste('grad_dim should be a vector of size (1, ', toString(dx), ').'))
      }
      if (any(grad_dim<0) || any(grad_dim>1) || any((grad_dim>0)*(grad_dim<1))){
        stop('The elements of grad_dim should be either 1 or 0.')
      }
      YgF = array(0, c(m, sum(grad_dim), dy))
      jj = 1
      for (d in 1:dx){
        if (grad_dim[i] > 0){
          XFNd = XFN[, d]
          XNd = XN[, d]
          RxfD = (Power*10^Theta[d])/(Xmax[d] - Xmin[d])*(replicate(m, XNd)-t(replicate(n, XFNd)))*Rxf
          YFN_der = t(RxfD)%*%(Rinv_YN - RinvFn%*%B)
          YgF[, jj, ] = t(t(YFN_der)*Yrange)
          jj = jj + 1
        }
      }
    }
  } else {
    XN0 = Model$CovFun$Parameters$XN0
    XFN = t(t(XFN) - XN0)
    YN0 = Model$CovFun$Parameters$YN0
    Alpha = Model$CovFun$Parameters$Alpha
    Rinv_YN = Model$CovFun$Parameters$Rinv_YN
    A = Model$CovFun$Parameters$A
    Beta = Model$CovFun$Parameters$Beta
    Gamma = Model$CovFun$Parameters$Gamma
    if (CorrType == 'LB'){
      Rxf = CorrMat_Vec(XN, XFN, CorrType, c(A, Beta, Gamma))
    } else {
      Rxf = CorrMat_Vec(XN, XFN, CorrType, c(A, Beta))
    }
    YFN = Fm%*%YN0 + t(Rxf)%*%Rinv_YN
    YF = t(t(YFN)*Yrange + Ymin)
    if (MSE_on){
      Rinv_Rxf = CppSolve(t(L), CppSolve(L, Rxf))
      MSE <- 1 - matrix(colSums(Rxf*Rinv_Rxf), m, 1) + Nug_opt
      MSE <- matrix(kronecker(matrix(diag(Alpha)*Yrange^2, dy, 1), MSE), m, dy)
    }
    if (YgF_on){
      A = 10^A - 1
      if (Gamma != 1){
        stop('The gradient can be calculated only if Gamma == 1.')
      }
      YgF = array(0, c(m, sum(grad_dim), dy))
      jj = 1
      for (d in 1:dx){
        if (grad_dim[d] > 0){
          XFNd = XFN[, d]
          XNd = XN[, d]
          RxfD = matrix(0, n, m)
          if (n >= m){
            for (i in 1:m){
              RxfD[, i] = 2*A[d]*Beta/(Xmax[d] - Xmin[d])*(XFNd[i]*(1 + sum(XFN[i, ]^2*A))^(Beta - 1) -
                          (XFNd[i] - XNd)*(1 + colSums((XFN[i, ] - t(XN))^2*A))^(Beta - 1))
            }
          } else{
            for (i in 1:n){
              RxfD[i, ] = 2*A[d]*Beta/(Xmax[d] - Xmin[d])*(XFNd*(1 + colSums(t(XFN^2)*A))^(Beta - 1) -
                          (XFNd - XNd[i])*(1 + colSums((t(XFN) - XN[i, ])^2*A))^(Beta - 1))
            }
          }

          YFN_der = t(RxfD)%*%Rinv_YN
          YgF[, jj, ] = t(t(YFN_der)*Yrange)
          jj = jj + 1
        }
      }
    }
  }
  Output = list(YF = YF, MSE = MSE, YgF = YgF)
  return(Output)
}
