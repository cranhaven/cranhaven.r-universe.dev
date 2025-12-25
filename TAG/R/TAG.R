TAG <- function(iniTAG, HighD  = FALSE, delta.threshold=-6){
  Res = iniTAG$y
  Input = iniTAG$X
  lambda.ini = iniTAG$lambda
  omega.ini = iniTAG$omega
  theta.ini = iniTAG$s
  delta.ini = iniTAG$delta

  d = ncol(Input)
  n = nrow(Input)
  if(HighD == FALSE){
      #constraint
      theta.rs <- rbind(diag(1,d),diag(-1,d))
      omega.rs <- rbind(diag(1,d),rep(1,d),rep(-1,d))
      delta.rs <- rbind(1,-1)
      lambda.rs <- rbind(1,-1)
      Amat <- t(as.matrix(bdiag(theta.rs,omega.rs,delta.rs,lambda.rs)))
      bvec <- c(theta.ini/5,-theta.ini*5,rep(0,d),c(1,-1),max(delta.ini-3, delta.threshold + 10^-6),
                -(max(min(delta.ini + 3, -.1),-4)),(lambda.ini-0.5),-(lambda.ini+0.5)) - 10^-6

      optim.res <- constrOptim(theta=c(theta.ini, omega.ini, max(delta.ini, -5), lambda.ini),
                               f=s2anddelta.rcpp,
                      ui = t(Amat), ci = bvec, grad=NULL, outer.eps = 10^-3,
                      Cov=Input, ORes = Res)

      para = c(theta.ini, omega.ini, delta.ini, lambda.ini)

      est.para <- optim.res$par
      theta.new <- est.para[1:d]
      omega.new <- est.para[(d+1):(2*d)]
      delta.new <- max(est.para[(2*d+1)], delta.threshold)
      lambda.new <- est.para[(2*d+2)]
      kappa <- "Not for this case"

    }else{
      #, control = list(abstol = 10^-3)
      optim.res <- optim(par=c(1, max(delta.ini, -5)), fn=s2anddelta.big.rcpp,
                         gr=s2anddelta.big.firstdf.rcpp,
                               method = "L-BFGS-B",
                               lower = c(0.1, max(delta.ini-3, delta.threshold)),
                               upper = c(5, min(delta.ini + 3, -1)),
                               Cov=Input, ORes = Res,
                               theta.est=theta.ini,omega.est=omega.ini,lambda.est=lambda.ini)

      est.para <- optim.res$par
      theta.new <- theta.ini
      omega.new <- omega.ini
      delta.new <- max(est.para[2], delta.threshold)
      lambda.new <- lambda.ini
      kappa <- est.para[1]
    }


    if(lambda.new == 0){
      Y.t <- log(Res)
      J <- sum(log(1/Res))
    }else{
      Y.t <- (Res^(lambda.new) - 1)/lambda.new
      J <- sum(log(Res^(lambda.new - 1)))
    }
  obj <- list(omega = omega.new, s = theta.new, lambda=lambda.new,
                  delta = delta.new, kappa = kappa, ty = Y.t, X = Input)
  class(obj) <- "TAG"
  return(obj)

}

s2anddelta.rcpp <- function(para, ORes, Cov){
  n <- nrow(Cov)
  d <- ncol(Cov)
  if(para[(2*d+2)]  == 0){
    TRes <- log(ORes)
  }else{
    TRes <- (ORes^(para[(2*d+2)]) - 1)/para[(2*d+2)]
  }
  one <- matrix(1, nrow=nrow(Cov))
  Ide <- diag(1, nrow(Cov))
  R <- calcADDR3(Cov,theta=para[c(1:d)], omega=para[c((d+1):(2*d))])
  R <- R + 10^(para[(2*d+1)])*Ide
  inv.R = rcppeigen_invert_matrix(R)
  mu=drop(t(one)%*%inv.R%*%TRes/(t(one)%*%inv.R%*%one))
  epsilon = (t(TRes-mu)%*%inv.R%*%(TRes-mu))/n
  eigvalues <- as.vector(getEigen(R))
  val = sum(log(eigvalues)) + n*log(epsilon)   - 2*(para[(2*d+2)]-1)*sum(log(ORes))
  return(val)
}

s2anddelta.big.rcpp <- function(para, ORes, Cov,theta.est,omega.est,lambda.est){
  n <- nrow(Cov)
  if(lambda.est == 0){
    TRes <- log(ORes)
  }else{
    TRes <- (ORes^(lambda.est) - 1)/lambda.est
  }
  one <- matrix(1, nrow=nrow(Cov))
  Ide <- diag(1, nrow(Cov))
  R <- calcADDR3(Cov,theta=theta.est, omega=omega.est)
  R <- R + 10^(para[(2)])*Ide
  inv.R = rcppeigen_invert_matrix(R)
  mu=drop(t(one)%*%inv.R%*%TRes/(t(one)%*%inv.R%*%one))
  epsilon = (t(TRes-mu)%*%inv.R%*%(TRes-mu))/n
  eigvalues <- as.vector(getEigen(R))
  val =  sum(log(eigvalues)) + n*log(epsilon)  - 2*(lambda.est-1)*sum(log(ORes))
  return(val)
}


s2anddelta.big.firstdf.rcpp <- function(para, ORes, Cov,theta.est,omega.est,lambda.est){
  n <- nrow(Cov)
  if(lambda.est == 0){
    TRes <- log(ORes)
  }else{
    TRes <- (ORes^(lambda.est) - 1)/lambda.est
  }
  one <- matrix(1, nrow=nrow(Cov))
  Ide <- diag(1, nrow(Cov))

  R <- calcADDR3(Cov,theta=theta.est, omega=omega.est)
  D <- calcDR(Cov,theta=theta.est, phiest = para[1])
  ComR <- R + 10^(para[(2)])*Ide
  Rinv = rcppeigen_invert_matrix(ComR)
  mu=drop(t(one)%*%Rinv%*%TRes/(t(one)%*%Rinv%*%one))
  sigma2 = (t(TRes-mu)%*%Rinv%*%(TRes-mu))/n
  coef=Rinv%*%(TRes-mu)
  gr1=-1/sigma2*t(coef)%*%(R*D)%*%coef + sum(diag(Rinv%*%(R*D)))
  gr2=-1/sigma2*t(coef)%*%(10^(para[(2)])*log(10)*Ide)%*%coef+sum(diag(Rinv%*%( 10^(para[(2)])*log(10)*Ide)))
  return(c(gr1,gr2))

}

