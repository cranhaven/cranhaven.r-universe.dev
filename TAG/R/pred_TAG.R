pred.TAG = function(object, newX){
  Input = object$X
  Y.tra = object$ty
  theta.est =  object$s
  omega.est =  object$omega
  delta.est = object$delta
  lambda.est  = object$lambda

  d <- ncol(Input)
  n <- nrow(Input)
  Ide <- diag(1, n)
  one <- rep(1, n)
  R <- matrix(0,nrow=n,ncol=n)
  for(i in 1:d){
    R <- R +  ((omega.est[i])*exp(-(1/(theta.est[i]^2))*(as.matrix(dist(Input[,i], diag = TRUE, upper = TRUE))^2)))
  }
  R <- R + 10^(delta.est) * Ide
  eig=eigen(R)
  inv.R=eig$vec%*%diag(1/eig$val)%*%t(eig$vec)

  mu.hat=drop(t(one)%*%inv.R%*%Y.tra/(t(one)%*%inv.R%*%one))
  sigma2.hat=1/n*t(Y.tra-mu.hat)%*%inv.R%*%(Y.tra-mu.hat)
  coef =   inv.R %*% (Y.tra - as.numeric(mu.hat))

  y.hat <- mu.hat + t(r.TAG(est.temp=newX, Cov=Input,theta.temp=theta.est, omega.temp=omega.est)) %*% coef


  if(lambda.est == 0){
    pre.Y <- exp(y.hat)
  }else{
    pre.Y <- ((y.hat*lambda.est + 1)^(1/lambda.est))
    pre.Y[y.hat*lambda.est < - 1] <- 0
  }
  (as.vector(pre.Y))
}


r.TAG=function(est.temp, Cov, theta.temp, omega.temp){
  vec=apply(Cov,1,function(vec){
    r1 <- rep(0,length(est.temp[,1]))
    for(i in 1:ncol(Cov)){
      r1 <- r1 + ((omega.temp[i])*exp(-(1/(theta.temp[i]^2)*((vec[i]-est.temp[,i])^2))))
    }
    (r1)
  })
  return(t(vec))
}
