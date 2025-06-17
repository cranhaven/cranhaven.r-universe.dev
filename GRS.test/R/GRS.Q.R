GRS.Q <-
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
  
  sigma = crossprod(e.mat)/(T-K-1); #sigma = crossprod(e.mat)/(T) Use Unbiased estimator as in GRS
  alpha = matrix(b.mat[,1],nrow=N)
  
  factor.mean = t(matrix(colMeans(factor.mat),nrow=K,ncol=T))
  omega = crossprod(factor.mat-factor.mean)/(T-1); #omega = crossprod(factor.mat-factor.mean)/(T) Use Unbiased estimator as in GRS
  
  tem1 = t(alpha) %*% solve(sigma) %*% alpha
  tem2 = 1 + factor.mean[1,, drop=FALSE] %*% solve(omega) %*% t(factor.mean[1,, drop=FALSE])
  tem3 = T/N; tem4 = (T-N-K)/(T-K-1)
  
  F = tem3*tem4 *(tem1/tem2)
  
  
  return(GRS.stat=F)
}
