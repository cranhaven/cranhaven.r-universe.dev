symmetric.mat = function(Omega){
  pm = dim(Omega)[1]
  Z = array(rep(0, 2*pm^2), dim=c(pm,pm,2))
  Z[,,1] = Omega
  Z[,,2] = t(Omega)
  Omega.sym = apply(Z, 1:2, function(x) x[which.min(abs(x))])
  return(Omega.sym)
}

S_soft = function(z,lambda){
  # S_soft: single lasso shrinkage estimate
  norm.z = sqrt(sum(z^2))
  if(norm.z!=0){
    n.x = 1 - lambda/norm.z
    rho = n.x*(n.x > 0)*z
  } else{
    rho = z
  }
  return(rho)
}

S_soft.vec = function(z,lambda,ej=rep(1,length(z))){
  # S_soft.vec: single lasso shrinkage estimate for a vector
  n.z = abs(z) - lambda*ej
  return(sign(z) * (n.z > 0) * n.z)
}

BIC.value = function(S.hat.A, delta.hat, Theta.hat, n=100, adjust=F){
  pm = dim(S.hat.A)[1]
  deltaI = delta.hat + diag(pm)
  fitness = 0.5*sum(diag(t(Theta.hat) %*% S.hat.A %*% Theta.hat)) - sum(diag( t(deltaI) %*%  Theta.hat))
  degree = sum(Theta.hat != 0) - pm
  if(adjust){Cn = log(n*pm)} else {Cn = 1}
  BIC.penalty = Cn*degree*log(n)  / n
  BICvalue = fitness + BIC.penalty

  return(list(BIC=BICvalue, fitness=fitness, BIC.penalty=BIC.penalty,degree=degree))
}

Separate.fit = function(t.data, lambda.vec, normalize){
  warning("The sepa method has not been included in the current version of this R package to circumvent code ownership issues.")
}
