TAAG <- function(parTAG, nu.est,
                 adj.nu = FALSE){

  Y.t = parTAG$ty
  Input = parTAG$X
  d <- ncol(Input)
  n <- nrow(Input)
  lam.new = parTAG$lambda
  omega.new = parTAG$omega
  theta.new = parTAG$s
  delta.new = parTAG$delta
  eta.ini = -log((10^delta.new)/(1+10^delta.new))#c(-log((10^delta.new)/(1+10^delta.new)),-log(1-1/d))

  phi.ini= 1
  lb.phi=10^-6
  ub.phi=20
  if(adj.nu == FALSE){
    phi.ini = lb.phi = ub.phi = 1
  }

  value.n <- rep(0,length(eta.ini))
  para.m <- matrix(0,ncol=2,nrow=length(eta.ini))
  for(kkk in 1:length(eta.ini)){
      temp <- optim(par=c(eta.ini[kkk], phi.ini),
                    fn=MLStep2onlyeta.2.rcpp, gr=MLStep2onlyeta.firstdf.2.rcpp,
                    method = c("L-BFGS-B"),
                    lower= c(10^-6, lb.phi), upper=c(20, ub.phi),
                    TRes=Y.t, Cov=Input, omega.est=omega.new,
                    theta.est=theta.new, theta.est2=nu.est,
                    delta.est=delta.new,tune.n=-6)
      value.n[kkk] <- temp$value
      para.m[kkk,] <- temp$par
  }
  un.posterior.value <- value.n[which.min(value.n)]
  para <- as.numeric(para.m[which.min(value.n),])
  para <- c(theta.new,exp(-para[1]),para[2])

  obj <- list(lambda = lam.new, omega = omega.new, s = theta.new, nu = nu.est,
              eta = para[(d+1)], phi = para[(d+2)],   obj.fun = un.posterior.value,
              ty = Y.t, X = Input)
  class(obj) <- "TAAG"
  return(obj)
}



MLStep2onlyeta.2.rcpp <- function(para, TRes, Cov, omega.est,
                           theta.est, theta.est2,
                           delta.est, tune.n = -6){
  one <- matrix(1, nrow=nrow(Cov))
  Ide <- diag(1, nrow(Cov))
  R <- calcADDR3(Cov,theta=theta.est, omega=omega.est)
  L <- calcProdR(Cov,theta=(para[2]*theta.est2))
  L <- exp(L)


  ComR <- (1-exp(-para[1]))*R +  exp(-para[1])*L
  ComR <- ComR + (10^(tune.n))*Ide
  eigvalues = as.vector(getEigen(ComR))
  inv.ComR = rcppeigen_invert_matrix(ComR)
  mu=drop(t(one)%*%inv.ComR%*%TRes/(t(one)%*%inv.ComR%*%one))
  tausq = t(TRes-mu)%*%inv.ComR%*%(TRes-mu)/nrow(Cov)
  val= sum(log(eigvalues)) + nrow(Cov)*log(tausq) - 2*log((exp(-para[1])^(10^(delta.est)))*(1-exp(-para[1])))
  return(val)
}

MLStep2onlyeta.firstdf.2.rcpp <- function(para, TRes, Cov, omega.est,
                                     theta.est,theta.est2,delta.est,
                                     tune.n=-8){
  one <- matrix(1, nrow=nrow(Cov))
  Ide <- diag(1, nrow(Cov))
  R <- calcADDR3(Cov,theta=theta.est, omega=omega.est)
  L <- calcProdR(Cov,theta=para[2]*theta.est2)
  L <- exp(L)
  D <- calcDR(Cov,theta=theta.est2, phiest = para[2])

  ComR <-  (1-exp(-para[1]))*R +  exp(-para[1])*L
  ComR <- ComR + (10^(tune.n))*Ide


  eigvalues = as.vector(getEigen(ComR))
  Rinv = rcppeigen_invert_matrix(ComR)
  mu=drop(t(one)%*%Rinv%*%TRes/(t(one)%*%Rinv%*%one))
  sigma2 = t(TRes-mu)%*%Rinv%*%(TRes-mu)/nrow(Cov)
  coef=Rinv%*%(TRes-mu)
  gr1=-1/sigma2*t(coef)%*%( exp(-para[1])*R - exp(-para[1])*L )%*%coef+sum(diag(Rinv%*%(exp(-para[1])*R - exp(-para[1])*L)))  -
    2*(10^(delta.est)*exp(-para[1])+ exp(-para[1]) -10^(delta.est))*exp(-para[1]*(10^(delta.est)))/(exp(-(10^(delta.est))*para[1])*(1-exp(-para[1])))
  gr2=-1/sigma2*t(coef)%*%( exp(-para[1])*L*D)%*%coef+sum(diag(Rinv%*%( exp(-para[1])*L*D)))
  return(c(gr1,gr2))

}
