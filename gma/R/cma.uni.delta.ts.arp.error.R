cma.uni.delta.ts.arp.error <-
function(dat,delta=0,p=1,conf.level=0.95,var.asmp=TRUE)
{
  Z<-matrix(dat$Z,ncol=1)
  M<-matrix(dat$M,ncol=1)
  R<-matrix(dat$R,ncol=1)
  
  n<-nrow(Z)
  
  z.alpha<-qnorm(1-(1-conf.level)/2)
  
  # time series: AR(p) autoregressive errors
  Zx<-apply(matrix(1:p,ncol=1),1,function(x){return(Z[(p+1-x):(n-x)])})
  Mx<-apply(matrix(1:p,ncol=1),1,function(x){return(M[(p+1-x):(n-x)])})
  Rx<-apply(matrix(1:p,ncol=1),1,function(x){return(R[(p+1-x):(n-x)])})
  
  Zy<-matrix(Z[-(1:p),1],ncol=1)
  My<-matrix(M[-(1:p),1],ncol=1)
  Ry<-matrix(R[-(1:p),1],ncol=1)
  
  ###########################################################
  # total effect model
  X<-cbind(Zy,Zx,Mx,Rx)
  fit.M<-lm(My~0+X)
  fit.R2<-lm(Ry~0+X)
  
  beta.M<-coef(fit.M)
  beta.R2<-coef(fit.R2)
  
  Sigma.B.hat<-(t(cbind(My-X%*%beta.M,Ry-X%*%beta.R2))%*%(cbind(My-X%*%beta.M,Ry-X%*%beta.R2)))/(n-1)
  # sigma12.hat<-Sigma.B.hat[1,1]
  # sigma22.hat<-det(Sigma.B.hat)/(Sigma.B.hat[1,1]*(1-delta^2))
  # Sigma.hat<-matrix(c(sigma12.hat,delta*sqrt(sigma12.hat*sigma22.hat),delta*sqrt(sigma12.hat*sigma22.hat),sigma22.hat),2,2)
  
  # Variance of beta.M and beta.R2
  beta.M.var<-Sigma.B.hat[1,1]*solve(t(X)%*%X)
  beta.R2.var<-Sigma.B.hat[2,2]*solve(t(X)%*%X)
  ###########################################################
  
  ###########################################################
  # coefficient estimate given delta
  
  # projection matrices
  P.M<-My%*%solve(t(My)%*%My)%*%t(My)
  O.M<-diag(rep(1,n-p))-P.M
  P.MX<-O.M%*%X%*%solve(t(X)%*%O.M%*%X)%*%t(X)%*%O.M
  P.X<-X%*%solve(t(X)%*%X)%*%t(X)
  O.X<-diag(rep(1,n-p))-P.X
  
  sigma12.hat<-(t(My)%*%O.X%*%My/(n-p))[1,1]
  sigma22.hat<-(t(Ry)%*%(O.M-P.MX)%*%Ry/((n-p)*(1-delta^2)))[1,1]
  Sigma.hat<-matrix(c(sigma12.hat,delta*sqrt(sigma12.hat*sigma22.hat),delta*sqrt(sigma12.hat*sigma22.hat),sigma22.hat),2,2)
  
  kappa.hat<-delta*sqrt(sigma22.hat/sigma12.hat)
  
  theta1.hat<-solve(t(X)%*%X)%*%t(X)%*%My
  theta2.hat<-solve(t(X)%*%O.M%*%X)%*%t(X)%*%O.M%*%Ry+kappa.hat*theta1.hat
  B.hat<-(solve(t(My)%*%My)%*%t(My)%*%(diag(rep(1,n-p))-X%*%solve(t(X)%*%O.M%*%X)%*%t(X)%*%O.M)%*%Ry-kappa.hat)[1,1]
  
  # C'
  C2.hat<-coef(fit.R2)[1]
  
  # A, B and C
  A.hat<-theta1.hat[1,1]
  C.hat<-theta2.hat[1,1]
  ABp.hat<-A.hat*B.hat
  ABd.hat<-C2.hat-C.hat
  
  # transition matrix of error
  W.hat<-matrix(NA,2*p,2)
  W.hat[seq(2,2*p,by=2),c(1,2)]<-cbind(theta1.hat[(2*p+2):(3*p+1)],theta2.hat[(2*p+2):(3*p+1)])
  W.hat[seq(1,2*p-1,by=2),1]<-theta1.hat[(p+2):(2*p+1)]+theta1.hat[(2*p+2):(3*p+1)]*B.hat
  W.hat[seq(1,2*p-1,by=2),2]<-theta2.hat[(p+2):(2*p+1)]+theta2.hat[(2*p+2):(3*p+1)]*B.hat
  
  phi.hat<-cbind(theta1.hat[2:(p+1)],theta2.hat[2:(p+1)])
  colnames(phi.hat)<-c("phi1","phi2")
  
  if(var.asmp)
  {
    J1<-kronecker(diag(rep(1,p)),c(1,0))
    J2<-kronecker(diag(rep(1,p)),c(0,1))
    if(p==1)
    {
      Fm<-t(W.hat)
    }else
    {
      Fm<-rbind(cbind(t(W.hat)),cbind(diag(rep(1,2*(p-1))),matrix(0,2*(p-1),1))) 
    }
    Xi<-matrix(0,2*p,2*p)
    Xi[1:2,1:2]<-Sigma.hat
    Pi<-matrix(solve(diag(rep(1,(2*p)^2))-kronecker(Fm,Fm))%*%c(Xi),2*p,2*p)
    
    # Xt*Xt
    q<-mean(Z)
    ZZ<-q*diag(rep(1,p))
    ZM<-A.hat*q*diag(rep(1,p))
    ZR<-(C.hat+ABp.hat)*q*diag(rep(1,p))
    MM<-A.hat^2*q*diag(rep(1,p))+t(J1)%*%Pi%*%J1
    MR<-A.hat*(C.hat+ABp.hat)*q*diag(rep(1,p))+B.hat*t(J1)%*%Pi%*%J1+t(J1)%*%Pi%*%J2
    RR<-(C.hat+ABp.hat)^2*q*diag(rep(1,p))+B.hat^2*t(J1)%*%Pi%*%J1+B.hat*t(J1)%*%Pi%*%J2+B.hat*t(J2)%*%Pi%*%J1+t(J1)%*%Pi%*%J1
    
    X.cov<-matrix(NA,3*p+1,3*p+1)
    X.cov[1,]<-c(q,rep(0,p),rep(0,p),rep(0,p))
    X.cov[2:(p+1),]<-cbind(rep(0,p),ZZ,ZM,ZR)
    X.cov[(p+2):(2*p+1),]<-cbind(rep(0,p),t(ZM),MM,MR)
    X.cov[(2*p+2):(3*p+1),]<-cbind(rep(0,p),t(ZR),t(MR),RR)
    
    # Xt*Mt
    psi11.hat<-matrix(W.hat[seq(1,2*p,by=2),1]-W.hat[seq(2,2*p,by=2),1]*B.hat,ncol=1)
    psi21.hat<-matrix(W.hat[seq(2,2*p,by=2),1],ncol=1)
    XM.cov<-matrix(NA,3*p+1,1)
    XM.cov[1,1]<-A.hat*q
    XM.cov[2:(p+1),1]<-ZZ%*%phi.hat[,1]+ZM%*%psi11.hat+ZR%*%psi21.hat
    XM.cov[(p+2):(2*p+1),1]<-t(ZM)%*%phi.hat[,1]+MM%*%psi11.hat+MR%*%psi21.hat
    XM.cov[(2*p+2):(3*p+1),1]<-t(ZR)%*%phi.hat[,1]+t(MR)%*%psi11.hat+RR%*%psi21.hat
    
    M.cov<-A.hat^2*q+Sigma.hat[1,1]+t(phi.hat[,1])%*%(ZZ%*%phi.hat[,1]+ZM%*%psi11.hat+ZR%*%psi21.hat)+
      t(psi11.hat)%*%(t(ZM)%*%phi.hat[,1]+MM%*%psi11.hat+MR%*%psi21.hat)+
      t(psi21.hat)%*%(t(ZR)%*%phi.hat[,1]+t(MR)%*%psi11.hat+RR%*%psi21.hat)
    
    dn1<-sigma12.hat*(1-delta^2)
    dn2<-sigma22.hat*(1-delta^2)
    Fisher.info<-rbind(cbind(X.cov/dn1,-kappa.hat*X.cov/dn2,-kappa.hat*XM.cov/dn2),
                       cbind(-kappa.hat*t(X.cov)/dn2,X.cov/dn2,XM.cov/dn2),
                       cbind(-kappa.hat*t(XM.cov)/dn2,t(XM.cov)/dn2,M.cov/dn2))*(n-p)
    
    # theta.var<-solve(Fisher.info)
    theta.var<-ginv(Fisher.info)
    
    theta1.var<-theta.var[1:(3*p+1),1:(3*p+1)]
    theta2.var<-theta.var[(3*p+2):(6*p+2),(3*p+2):(6*p+2)]
    B.var<-theta.var[6*p+3,6*p+3]
  }else
  {
    theta1.var<-solve(t(X)%*%X/sigma12.hat)
    theta2.var<-solve(t(X)%*%X/(sigma22.hat*(1-delta^2)))
    B.var<-((sigma22.hat*(1-delta^2))/t(M)%*%M)[1,1] 
  }
  
  C2.hat.se<-sqrt(beta.R2.var[1,1])
  
  A.hat.se<-sqrt(theta1.var[1,1])
  B.hat.se<-sqrt(B.var)
  C.hat.se<-sqrt(theta2.var[1,1])
  
  ABp.hat.se<-sqrt(A.hat^2*B.hat.se^2+B.hat^2*A.hat.se^2)
  ABd.hat.se<-sqrt(C2.hat.se^2+C.hat.se^2)
  
  cma.re<-matrix(NA,6,4)
  rownames(cma.re)<-c("A","C","B","C2","AB.p","AB.d")
  colnames(cma.re)<-c("Estimate","SE","LB","UB")
  cma.re[,1]<-c(A.hat,C.hat,B.hat,C2.hat,ABp.hat,ABd.hat)
  cma.re[,2]<-c(A.hat.se,C.hat.se,B.hat.se,C2.hat.se,ABp.hat.se,ABd.hat.se)
  cma.re[,3]<-cma.re[,1]-z.alpha*cma.re[,2]
  cma.re[,4]<-cma.re[,1]+z.alpha*cma.re[,2]
  D.hat<-matrix(c(A.hat,0,C.hat,B.hat),2,2)
  
  re<-list(Coefficients=cma.re,D=D.hat,Sigma=Sigma.hat,delta=delta,W=W.hat)
  
  return(re)
}
