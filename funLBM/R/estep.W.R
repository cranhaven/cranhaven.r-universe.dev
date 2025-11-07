estep.W <-
function(X,alpha,beta,mu,a,b,d,Q,Z,Wmat){
  ## Initialization
  K = length(alpha); L =length(beta)
  P = matrix(NA,ncol(X),L)
  ## Cost function computing
  for (l in 1:L){
    A = matrix(NA,nrow(X),ncol(X))
    for (k in 1:K){
      Xk = X[Z[,k]==1,,]
      if (sum(Z[,k])==1) A[Z[,k]==1,] = estep.W.cost(Xk,alpha,beta,mu,a,b,d,Q,k,l,Wmat)
      else A[Z[,k]==1,] = apply(Xk,2,estep.W.cost,alpha,beta,mu,a,b,d,Q,k,l,Wmat)
    }
    P[,l] = colSums(-1/2*A)
  }
  P = t(apply(P,1,function(x){t = c(); for (l in 1:L) t[l] = 1 / sum(exp(x - x[l])); t}))
}
