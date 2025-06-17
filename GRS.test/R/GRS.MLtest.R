GRS.MLtest <-
function(ret.mat,factor.mat){
  ret.mat=as.matrix(ret.mat); factor.mat=as.matrix(factor.mat)
  N=ncol(ret.mat); T=nrow(ret.mat); K = ncol(factor.mat)
  
  rmat=as.matrix(colMeans(factor.mat))
  mat=factor.mat
  V = cov(mat); V=V*(T-1)/(T)  # Use ML estimator as in GRS
  theta2 = t(rmat) %*% solve(V) %*% rmat
  
  rmat=rbind(as.matrix(colMeans(ret.mat)),as.matrix(colMeans(factor.mat)))
  mat=cbind(ret.mat,factor.mat)
  V = cov(mat); V=V*(T-1)/(T)  # Use ML estimator as in GRS
  thetas2 = t(rmat) %*% solve(V) %*% rmat
  
  tem3 = T/N; tem4 = (T-N-K)/(T-K-1)
  tem1= sqrt(1+thetas2); tem2= sqrt(1+theta2)
  F = tem3*tem4 *((tem1/tem2)^2 -1)
  p.F = pf(F,df1=N,df2=T-N-K,lower.tail=FALSE)
  ratio=sqrt(theta2/thetas2)
  
  colnames(F) = "GRS"; colnames(p.F) = "GRS"
  
  return(list(GRS.stat=F,GRS.pval=p.F,thetas=sqrt(thetas2),theta=sqrt(theta2),ratio=ratio))
}
