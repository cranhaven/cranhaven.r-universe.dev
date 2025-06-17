GRS.optimalboot <-
function(ret.mat,factor.mat,p=0.5,k=1,nboot=3000,wild=FALSE,Graph=TRUE){
  #set.seed(123456)
  ret.mat=as.matrix(ret.mat); factor.mat=as.matrix(factor.mat)
  N=ncol(ret.mat); T=nrow(ret.mat); K = ncol(factor.mat)
  M0=GRS.Q0(ret.mat,factor.mat)
  M1=GRS.Q1(ret.mat,factor.mat)
  
  b0=M0$coef; e0=M0$resid
  b1=M1$coef; e1=M1$resid
  
  rs0.mat=matrix(NA,ncol=N,nrow=T); rs1.mat=matrix(NA,ncol=N,nrow=T)
  
  dat0= as.matrix(factor.mat)
  one = matrix(1,nrow=T,ncol=1); dat1 = as.matrix(cbind( one,factor.mat))
  
  f.mat0=matrix(NA,nrow=nboot,ncol=1); f.mat1=matrix(NA,nrow=nboot,ncol=1)
  for(j in 1:nboot){
    
    if (wild==FALSE) {index=sample(1:T,size=T,replace=TRUE); e0mat=e0[index,]; e1mat=e1[index,]}
    if (wild==TRUE)  {e0mat = wild(T) * e0; e1mat = wild(T) * e1}
    
    for(i in 1:N) {
      rs0.mat[,i] = dat0 %*% t(b0[i,,drop=FALSE]) + e0mat[,i] 
      rs1.mat[,i] = dat1 %*% t(b1[i,,drop=FALSE]) + e1mat[,i] 
    }
    
    f.mat0[j,]=GRS.Q(rs0.mat,factor.mat)
    f.mat1[j,]=GRS.Q(rs1.mat,factor.mat)
    
    
  }
  
  cr1=quantile(f.mat0,probs=0.95)
  power1=mean(f.mat1 > cr1)
  
  alphavec=seq(0,1,0.00001)
  powervec=alphavec
  crvec=quantile(f.mat0,probs=1-alphavec)
  for(i in 1:length(crvec))powervec[i]=mean(f.mat1 > crvec[i])
  
  betavec=1-powervec     
  dd=cbind(alphavec,betavec,abs(p*alphavec+(1-p)*k*betavec))
  dd1= dd[dd[,1] == 0.05,]
  dd2= dd[dd[,3] == min(dd[,3]),]
  alphas=dd2[1];betas=dd2[2]; names(alphas) = NULL; names(betas) = NULL
  crits=quantile(f.mat0,probs=1-alphas); names(crits) = NULL

  if (Graph == TRUE) {
    par(mfrow=c(1,2))
    #Plotting densities under H0 and H1
    x=f.mat0; density=density(x)
    x1=f.mat1; density1=density(x1)
    plot(density,xlim=c(0,max(f.mat1)),lwd=2,main="Density functions under H0 and H1")
    points(density1,type="l",col=2,lwd=2)
    abline(v=crits,col="blue")
    
    plot(betavec,alphavec,type="l",xlim=c(0,1),col=1,lwd=2,ylab="alpha",xlab="beta",cex.lab=1.5,main="Optimal Level of Significance") 
    points(dd2[2],dd2[1],col=4,pch=15,cex=1.5)
    abline(v=seq(0,1,0.1), col="lightgray", lty="dotted")
    abline(h=seq(0,1,0.1), col="lightgray", lty="dotted")
    abline(h=0.05,col="red",lwd=2)
    par(mfrow=c(1,1))}
    
  return(list(opt.sig=alphas,opt.crit=crits,opt.beta=betas))}
