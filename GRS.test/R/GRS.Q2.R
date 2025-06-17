GRS.Q2 <-
function(ret.mat,factor.mat){
  ret.mat=as.matrix(ret.mat); factor.mat=as.matrix(factor.mat)
  N=ncol(ret.mat); T=nrow(ret.mat); K = ncol(factor.mat)
  
  e.mat = matrix(NA,ncol=N,nrow=T)
  b.mat = matrix(NA,ncol=K+1,nrow=N)
  
  one = matrix(1,nrow=T,ncol=1)
  dat = as.matrix(cbind( one,factor.mat))
  
  for(i in 1:N){
    ri = as.matrix(ret.mat[,i,drop=F])
    b = solve(t(dat) %*% dat) %*% t(dat) %*%  ri; 
    e = ri -  dat %*% b 
    b.mat[i,] =  b
    e.mat[,i] =  e
  }
  
  sigma = crossprod(e.mat)/(T-K-1);
  rmat=as.matrix(colMeans(factor.mat))
  mat=factor.mat
  V = cov(mat); V=V*(T-1)/(T)  # Use ML estimator as in GRS
  theta2 = t(rmat) %*% solve(V) %*% rmat
  
  rmat=rbind(as.matrix(colMeans(ret.mat)),as.matrix(colMeans(factor.mat)))
  mat=cbind(ret.mat,factor.mat)
  V = cov(mat); V=V*(T-1)/(T)  # Use ML estimator as in GRS
  thetas2 = t(rmat) %*% solve(V) %*% rmat
  ratio=sqrt(theta2/thetas2)
  
  lamda=(T/ratio^2)/(1+1/theta2) * (1-ratio^2)
  
  #amat=b.mat[,1,drop=FALSE]; tem = t(amat) %*% solve(sigma) %*% amat
  #lamda=(T/(1+theta2)) * tem
  return(list(lambda=lamda,coef=b.mat,resid=e.mat))
}
