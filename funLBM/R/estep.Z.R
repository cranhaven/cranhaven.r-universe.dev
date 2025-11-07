estep.Z <-
function(X,alpha,beta,mu,a,b,d,Q,W,Wmat){
  ## Initialization
  K = length(alpha); L =length(beta)
  P = matrix(NA,nrow(X),K)
  ## Cost function computing
  for (k in 1:K){
    A = matrix(NA,nrow(X),ncol(X))
    for (l in 1:L){
      Xl = X[,W[,l]==1,]
      if (sum(W[,l])==1) A[,W[,l]==1] = estep.Z.cost(Xl,alpha,beta,mu,a,b,d,Q,k,l,Wmat)
      else A[,W[,l]==1] = apply(Xl,2,estep.Z.cost,alpha,beta,mu,a,b,d,Q,k,l,Wmat)
    }
    P[,k] = rowSums(-1/2*A)
  }
  P = t(apply(P,1,function(x){t = c(); for (k in 1:K) t[k] = 1 / sum(exp(x - x[k])); t}))
}
