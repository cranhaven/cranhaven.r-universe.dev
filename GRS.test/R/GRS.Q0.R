GRS.Q0 <-
function(ret.mat,factor.mat){
  ret.mat=as.matrix(ret.mat); factor.mat=as.matrix(factor.mat)
  N=ncol(ret.mat); T=nrow(ret.mat); K = ncol(factor.mat)
  
  e.mat = matrix(NA,ncol=N,nrow=T)
  b.mat = matrix(NA,ncol=K,nrow=N)
  
  dat = factor.mat
  
  for(i in 1:N){
    ri = as.matrix(ret.mat[,i,drop=F])
    b = solve(t(dat) %*% dat) %*% t(dat) %*%  ri; 
    e = ri -  dat %*% b 
    b.mat[i,] =  b
    e.mat[,i] =  e
    }

  return(list(GRS.stat=F,coef=b.mat,resid=e.mat))
}
