cma.uni.delta <-
function(dat,delta=0,conf.level=0.95)
{
  Z<-matrix(dat$Z,ncol=1)
  M<-matrix(dat$M,ncol=1)
  R<-matrix(dat$R,ncol=1)
  
  n<-nrow(Z)
  
  z.alpha<-qnorm(1-(1-conf.level)/2)
  
  ########################################
  # total effect model
  X<-cbind(Z,M)
  fit.M<-lm(M~0+Z)
  fit.R2<-lm(R~0+Z)
  
  beta.M<-coef(fit.M)
  beta.R2<-coef(fit.R2)
  
  Sigma.B.hat<-(t(cbind(M-Z%*%beta.M,R-Z%*%beta.R2))%*%cbind(M-Z%*%beta.M,R-Z%*%beta.R2))/n
  sigma12.hat<-Sigma.B.hat[1,1]
  sigma22.hat<-det(Sigma.B.hat)/(Sigma.B.hat[1,1]*(1-delta^2))
  Sigma.hat<-matrix(c(sigma12.hat,delta*sqrt(sigma12.hat*sigma22.hat),delta*sqrt(sigma12.hat*sigma22.hat),sigma22.hat),2,2)
  
  # variance of beta.M and beta.R2
  beta.M.var<-Sigma.B.hat[1,1]*solve(t(Z)%*%Z)
  beta.R2.var<-Sigma.B.hat[2,2]*solve(t(X)%*%X)
  #########################################
  
  #########################################
  # coefficient estimate given delta
  
  # C'
  C2.hat<-coef(fit.R2)[1]
  C2.hat.se<-sqrt(beta.R2.var[1,1])
  
  # A, B, and C
  zz<-(t(Z)%*%Z)[1,1]
  zm<-(t(Z)%*%M)[1,1]
  mz<-(t(M)%*%Z)[1,1]
  zr<-(t(Z)%*%R)[1,1]
  mm<-(t(M)%*%M)[1,1]
  mr<-(t(M)%*%R)[1,1]
  
  A.hat<-zm/zz
  C.hat<-(mm*zr-zm*mr)/(zz*mm-mz*zm)+(delta*sqrt(sigma22.hat/sigma12.hat))*(zm/zz)
  B.hat<-(zz*mr-mz*zr)/(zz*mm-mz*zm)-(delta*sqrt(sigma22.hat/sigma12.hat))
  # inverse of Fisher infromation
  coef.cov<-matrix(0,3,3)
  coef.cov[1,1]<-n*sigma12.hat/zz
  coef.cov[1,2]=coef.cov[2,1]<-n*delta*sqrt(sigma12.hat*sigma22.hat)/zz
  coef.cov[2,2]<-sigma22.hat*(A.hat^2*zz+n*sigma12.hat-delta^2*A.hat^2*zz)/(sigma12.hat*zz)
  coef.cov[2,3]=coef.cov[3,2]<--sigma22.hat*(1-delta^2)*A.hat/sigma12.hat
  coef.cov[3,3]<-sigma22.hat*(1-delta^2)/sigma12.hat
  
  A.hat.se<-sqrt(coef.cov[1,1]/n)
  C.hat.se<-sqrt(coef.cov[2,2]/n)
  B.hat.se<-sqrt(coef.cov[3,3]/n)
  
  ABp.hat<-A.hat*B.hat
  ABp.hat.se<-sqrt(A.hat^2*B.hat.se^2+B.hat^2*A.hat.se^2)
  ABd.hat<-C2.hat-C.hat
  ABd.hat.se<-sqrt(C2.hat.se^2+C.hat.se^2)
  
  cma.re<-matrix(NA,6,4)
  rownames(cma.re)<-c("A","C","B","C2","ABp","ABd")
  colnames(cma.re)<-c("Estimate","SE","LB","UB")
  cma.re[,1]<-c(A.hat,C.hat,B.hat,C2.hat,ABp.hat,ABd.hat)
  cma.re[,2]<-c(A.hat.se,C.hat.se,B.hat.se,C2.hat.se,ABp.hat.se,ABd.hat.se)
  cma.re[,3]<-cma.re[,1]-z.alpha*cma.re[,2]
  cma.re[,4]<-cma.re[,1]+z.alpha*cma.re[,2]
  
  D.hat<-matrix(c(A.hat,0,C.hat,B.hat),2,2)
  
  re<-list(Coefficients=cma.re,D=D.hat,Sigma=Sigma.hat,delta=delta)
  
  return(re)
}
