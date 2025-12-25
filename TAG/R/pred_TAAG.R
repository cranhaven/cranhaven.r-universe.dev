pred.TAAG = function(object, newX, predict.CI = FALSE,  zalpha = 1.96){
  omega.new = object$omega
  lambda.new = object$lambda
  theta.new = object$s
  theta.new2 = object$nu
  phi.est = object$phi
  eta.est = object$eta
  Input = object$X
  Y.t = object$ty

  d <- ncol(Input)
  n <- nrow(Input)
  Ide <- diag(1, n)
  one <- rep(1, n)
  R <- calcADDR3(Input,theta=theta.new, omega=omega.new)
  L <- calcProdR(Input,theta=phi.est*theta.new2)
  L <- exp(L)

  R <-   (1-eta.est)*R + eta.est*L
  R <- R + 10^(-6) * Ide

  #eigvalues = as.vector(getEigen(R))
  inv.R = rcppeigen_invert_matrix(R)
  #eig=eigen(R)
  #inv.R=eig$vec%*%diag(1/eig$val)%*%t(eig$vec)

  mu.hat=drop(t(one)%*%inv.R%*%Y.t/(t(one)%*%inv.R%*%one))
  sigma2.hat=1/n*t(Y.t-mu.hat)%*%inv.R%*%(Y.t-mu.hat)
  coef =   inv.R %*% (Y.t - as.numeric(mu.hat))

  pre.Y=y.hat=numeric(nrow(newX))
  y.hat <- mu.hat + t(rpred2(est=newX, Cov=Input, theta.est=theta.new, theta.est2=theta.new2,
                             omega.est=omega.new, phi = phi.est, eta = eta.est)) %*% coef

  if(lambda.new == 0){
    pre.Y <- exp(y.hat)
  }else{
    pre.Y <- ((y.hat*lambda.new + 1)^(1/lambda.new))
    pre.Y[y.hat*lambda.new < - 1] <- 0
  }

  if(predict.CI == FALSE){
    return(list(Prediction=pre.Y))
  }else{
    var.test <- drop(sigma2.hat)*(1-diag(t(rpred2(est=newX,Cov=Input,theta.est=theta.new,theta.est2=theta.new2,
                                                  omega.est=omega.new, phi = phi.est, eta = eta.est)) %*%
                                           inv.R  %*% rpred2(est=newX,Cov=Input,theta.est=theta.new,theta.est2=theta.new2,
                                                             omega.est=omega.new, phi = phi.est, eta = eta.est)))
    var.test[var.test < 0] <- 0
    if(lambda.new == 0){
      CILB <- exp(y.hat - zalpha*sqrt(var.test))
      CILB[y.hat - zalpha*sqrt(var.test) < 0] <- 0
      CIUB <- exp(y.hat + zalpha*sqrt(var.test))
      CIUB[y.hat + zalpha*sqrt(var.test) < 0] <- 0
    }else{
      CILB <- (((y.hat - zalpha*sqrt(var.test))*lambda.new + 1)^(1/lambda.new))
      CIUB <- (((y.hat + zalpha*sqrt(var.test))*lambda.new + 1)^(1/lambda.new))
    }
    return(list(Prediction=pre.Y, ConfidenceLB=CILB, ConfidenceUB=CIUB))
  }

}

rpred2=function(est,Cov,theta.est,theta.est2,omega.est,phi,eta){
  vec=apply(Cov,1,function(vec){
    r1 <- rep(0,length(est[,1]))
    r2 <- rep(1,length(est[,1]))
    for(i in 1:ncol(Cov)){
      r1 <- r1 + ((omega.est[i])*exp(-(1/(theta.est[i]^2)*((vec[i]-est[,i])^2))))
      r2 <- r2 * (exp(-(1/((phi*theta.est2[i])^2))*((vec[i]-est[,i])^2)))
    }

    ((1-eta)*r1 + eta*r2)
  })
  return(t(vec))
}
