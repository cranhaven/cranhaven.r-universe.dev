rsal <-
function(n,p,alpha,sig,mu){
  Y <- matrix(mvrnorm(n,rep(0,p),sig),nrow=n,ncol=p,byrow=TRUE)
  w <- rexp(n)
  alpha <- matrix(alpha,nrow=n,ncol=p,byrow=TRUE)
  X.1 <- w*alpha + sqrt(w)*Y
  mu <- matrix(mu,ncol=p,nrow=n,byrow=TRUE)
  X <- X.1 + mu
  return(X)
}
