GRS.optimalbootweight <-
function(ret.mat,factor.mat,p=0.5,k=1,nboot=3000,wild=FALSE,Graph=TRUE){
  #set.seed(123456)
  ret.mat=as.matrix(ret.mat); factor.mat=as.matrix(factor.mat)
  N=ncol(ret.mat); T=nrow(ret.mat); K = ncol(factor.mat)
  lambda.mat=matrix(NA,nrow=nboot,ncol=1)
  b.mat=array(NA,dim=c(N,K+1,nboot))
  e.mat=array(NA,dim=c(T,N,nboot))
  
  M1=GRS.Q2(ret.mat,factor.mat)
  b1=M1$coef; e1=M1$resid; ncp=M1$lambda
  rs1.mat=matrix(NA,ncol=N,nrow=T)
  
  one = matrix(1,nrow=T,ncol=1); dat1 = as.matrix(cbind( one,factor.mat))
  
  f.mat0=matrix(NA,nrow=nboot,ncol=1); f.mat1=matrix(NA,nrow=nboot,ncol=1)
  for(j in 1:nboot){
    
    if (wild==FALSE) {index=sample(1:T,size=T,replace=TRUE); e1mat=e1[index,]}
    if (wild==TRUE)  e1mat = wild(T) * e1
    
    for(i in 1:N) rs1.mat[,i] = dat1 %*% t(b1[i,,drop=FALSE]) + e1mat[,i] 
    
    M=GRS.Q2(rs1.mat,factor.mat)
    lambda.mat[j,1]=M$lambda
    b.mat[,,j]=M$coef
    e.mat[,,j]=M$resid
  }
  
  ncp.boot=lambda.mat
  tem=density(ncp.boot,n=5000); 
  tem1=cbind(tem$x,tem$y)
  tem3=quantile(ncp.boot[,1],probs=seq(0.1,0.9,0.1),type=1)
  tem4=matrix(NA,nrow=length(tem3),ncol=2)
  for (i in 1:length(tem3)){
    index=which.min(abs(tem1[,1]-tem3[i]))
    tem4[i,]=tem1[index,,drop=FALSE]}
  tem4[,2] = tem4[,2]/sum(tem4[,2])
  tem5=matrix(NA,nrow=length(tem3),ncol=ncol(ncp.boot))
  indexvec=numeric()
  for (i in 1:nrow(tem4)){
    index=which.min(abs(ncp.boot[,1]-tem4[i])); indexvec=c(indexvec,index)
    tem5[i,]=ncp.boot[index,,drop=FALSE]}
  w = tem4[,2]
  
  if(Graph==TRUE) plot(tem,main="Bootstrap Distribution of Lambda",xlab="ncp",lwd=2); abline(v=ncp,col="blue")
  
  M0=GRS.Q0(ret.mat,factor.mat)
  b0=M0$coef; e0=M0$resid
  rs0.mat=matrix(NA,ncol=N,nrow=T);
  dat0= as.matrix(factor.mat)
  
  f.mat0=matrix(NA,nrow=nboot,ncol=1);
  for(j in 1:nboot){
    
    if (wild==FALSE)  {index=sample(1:T,size=T,replace=TRUE); e0mat=e0[index,]}
    if (wild==TRUE)   e0mat = wild(T) * e0
    
    for(i in 1:N) rs0.mat[,i] = dat0 %*% t(b0[i,,drop=FALSE]) + e0mat[,i] 
    
    f.mat0[j,]=GRS.Q(rs0.mat,factor.mat)
  }
  
  alphavec=seq(0,1,0.00001)
  powervec=alphavec
  crvec=quantile(f.mat0,probs=1-alphavec)
  
  one = matrix(1,nrow=T,ncol=1); dat1 = as.matrix(cbind( one,factor.mat))
  alphasvec=numeric()
  for (jj in 1:nrow(tem5)){
    b1=b.mat[,,indexvec[jj]]
    e1=e.mat[,,indexvec[jj]]; e1=e1-colMeans(e1)
    
    f.mat1=matrix(NA,nrow=nboot,ncol=1)
    for(j in 1:nboot){
      
      if (wild==FALSE) {index=sample(1:T,size=T,replace=TRUE); e1s=e1[index,]}
      if (wild==TRUE)  e1s = wild(T) * e1
      
      for(i in 1:N) rs1.mat[,i] = dat1 %*% t(b1[i,,drop=FALSE]) + e1s[,i] 
      
      f.mat1[j,]=GRS.Q(rs1.mat,factor.mat)
    }
    
    for(i in 1:length(crvec))powervec[i]=mean(f.mat1 > crvec[i])
    betavec=1-powervec     
    dd=cbind(alphavec,betavec,abs(p*alphavec+(1-p)*k*betavec))
    dd1= dd[dd[,1] == 0.05,]
    dd2= dd[dd[,3] == min(dd[,3]),]
    alphas=dd2[1];betas=dd2[2]; names(alphas) = NULL; names(betas) = NULL
    alphasvec=c(alphasvec,alphas)
  }
  alphas=sum(alphasvec*w); cr1=quantile(f.mat0,probs=1-alphas); names(cr1) = NULL
  return(list(opt.sig=alphas,opt.crit=cr1))}
