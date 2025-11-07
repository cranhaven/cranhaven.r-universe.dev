compute.CompleteLikelihood <-
function(X,alpha,beta,mu,a,b,d,Q,Z,W,Wmat){
  K = length(alpha); L=length(beta); p = dim(X)[2]
  lik = sum(Z %*% t(t(log(alpha)))) + sum(W %*% t(t(log(beta))))
  for (l in 1:L){
    for (k in 1:K){
      if (sum(W[,l])==1 | sum(Z[,k])==1) Xkl = X[Z[,k]==1,W[,l]==1,]
      else Xkl = apply(X[Z[,k]==1,W[,l]==1,],3,cbind)
      if (is.vector(Xkl)) Xkl = t(Xkl)
      lik = lik - 1/2 *sum(estep.lik.cost(Xkl,alpha,beta,mu,a,b,d,Q,k,l,Wmat))
    }
  }
  lik
}
