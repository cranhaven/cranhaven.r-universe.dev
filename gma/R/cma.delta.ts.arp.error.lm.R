cma.delta.ts.arp.error.lm <-
function(dat,delta=0,p=1,max.itr=500,tol=1e-4,error.indep=FALSE,error.var.equal=FALSE,
                                    Sigma.update=FALSE,var.constraint=FALSE)
{
  N<-length(dat)
  K<-1
  
  ##################################################################
  # Estimate A, B, C and C' for each subject
  At=Bt=Ct=C2t<-matrix(NA,N,K)
  sigma1.hat=sigma2.hat<-matrix(NA,N,K)
  Wt<-array(NA,c(N,2*p,2))
  thetat<-matrix(NA,N,6*p+3)
  if(p==1)
  {
    colnames(thetat)<-c("A","phi1","psi11","psi21","C","phi2","psi12","psi22","B")
  }else
  {
    colnames(thetat)<-c("A",paste0(rep(c("phi1","psi11","psi21"),each=p),"_",rep(1:p,3)),
                        "C",paste0(rep(c("phi2","psi12","psi22"),each=p),"_",rep(1:p,3)),
                        "B")
  }
  n<-matrix(NA,N,K)
  for(i in 1:N)
  {
    dd<-dat[[i]]
    n[i,1]<-nrow(dd)
    
    re<-cma.uni.delta.ts.arp.error(dd,delta=delta,p=p,var.asmp=FALSE)
    
    At[i,1]<-re$Coefficients[1,1]
    Bt[i,1]<-re$Coefficients[3,1]
    Ct[i,1]<-re$Coefficients[2,1]
    C2t[i,1]<-re$Coefficients[4,1]
    Wt[i,,]<-c(re$W)
    
    Theta<-re$D
    
    phi<-matrix(NA,p,2)
    colnames(phi)<-c("phi1","phi2")
    phi[,1]<--Wt[i,seq(1,2*p-1,by=2),1]*Theta[1,1]-Wt[i,seq(2,2*p,by=2),1]*Theta[1,2]
    phi[,2]<--Wt[i,seq(1,2*p-1,by=2),2]*Theta[1,1]-Wt[i,seq(2,2*p,by=2),2]*Theta[1,2]
    
    Psi<-matrix(NA,2*p,2)
    Psi[seq(1,2*p-1,by=2),1]<-Wt[i,seq(1,2*p-1,by=2),1]-Wt[i,seq(2,2*p,by=2),1]*Theta[2,2]
    Psi[seq(2,2*p,by=2),1]<-Wt[i,seq(2,2*p,by=2),1]
    Psi[seq(1,2*p-1,by=2),2]<-Wt[i,seq(1,2*p-1,by=2),2]-Wt[i,seq(2,2*p,by=2),2]*Theta[2,2]
    Psi[seq(2,2*p,by=2),2]<-Wt[i,seq(2,2*p,by=2),2]
    
    theta1<-c(Theta[1,1],phi[,1],Psi[seq(1,2*p-1,by=2),1],Psi[seq(2,2*p,by=2),1])
    theta2<-c(Theta[1,2],phi[,2],Psi[seq(1,2*p-1,by=2),2],Psi[seq(2,2*p,by=2),2])
    
    thetat[i,]<-c(theta1,theta2,Bt[i,1])
    
    sigma1.hat[i,1]<-re$Sigma[1,1]
    sigma2.hat[i,1]<-re$Sigma[2,2]
  }
  ##################################################################
  
  ##################################################################
  # Confidence interval of variance estimate
  fit.A<-gls(At~1)
  fit.B<-gls(Bt~1)
  fit.C<-gls(Ct~1)
  
  if(is.matrix(var.constraint))
  {
    if(nrow(var.constraint)==3)
    {
      Lambda.confint<-var.constraint[1:3,]
      colnames(Lambda.confint)<-c("LB","UB")
      rownames(Lambda.confint)<-c("A","B","C")
    }else
    {
      warning("The number of intervals is not correct. The constraint intervals will be estimated instead.")
      Lambda.confint<-matrix(NA,3,2)
      colnames(Lambda.confint)<-c("LB","UB")
      rownames(Lambda.confint)<-c("A","B","C")
      Lambda.confint[1,]<-(intervals(fit.A)[[2]][c(1,3)])^2
      Lambda.confint[2,]<-(intervals(fit.B)[[2]][c(1,3)])^2
      Lambda.confint[3,]<-(intervals(fit.C)[[2]][c(1,3)])^2
    }
  }else
    if(var.constraint==TRUE)
    {
      Lambda.confint<-matrix(NA,3,2)
      colnames(Lambda.confint)<-c("LB","UB")
      rownames(Lambda.confint)<-c("A","B","C")
      Lambda.confint[1,]<-(intervals(fit.A)[[2]][c(1,3)])^2
      Lambda.confint[2,]<-(intervals(fit.B)[[2]][c(1,3)])^2
      Lambda.confint[3,]<-(intervals(fit.C)[[2]][c(1,3)])^2
    }else
    {
      Lambda.confint<-NULL
    }
  ##################################################################
  
  ##################################################################
  # Covariance estimate
  Lambda.hat<-matrix(0,3,3)
  if(!error.var.equal)
  {
    if(error.indep)
    {
      fit.A<-lm(At~1)
      fit.B<-lm(Bt~1)
      fit.C<-lm(Ct~1)
      
      Afix<-coef(fit.A)
      Bfix<-coef(fit.B)
      Cfix<-coef(fit.C)
      b.hat<-c(Afix,Bfix,Cfix)
      
      Lambda.hat[1,1]<-(summary(fit.A)$sigma)^2
      Lambda.hat[2,2]<-(summary(fit.B)$sigma)^2
      Lambda.hat[3,3]<-(summary(fit.C)$sigma)^2
    }else
    {
      fit.b<-lm(cbind(At,Bt,Ct)~1)
      aov.b<-Anova(fit.b)
      b.hat<-as.vector(coef(fit.b))
      Lambda.hat<-aov.b$SSPE/aov.b$error.df
    }
  }else
  {
    if(error.indep==TRUE)
    {
      bt<-rbind(At,Bt,Ct)
      group<-c(rep("A",length(At)),rep("B",length(Bt)),rep("C",length(Ct)))
      fit<-lm(bt~group)
      b.hat<-as.vector(by(bt[,1],group,mean,na.rm=TRUE))
      diag(Lambda.hat)<-rep((summary(fit)$sigma)^2,3)
    }else
    {
      warning("This variance structure is not valid! The errors are assumed to be independent.")
      bt<-rbind(At,Bt,Ct)
      group<-c(rep("A",length(At)),rep("B",length(Bt)),rep("C",length(Ct)))
      fit<-lm(bt~group)
      b.hat<-as.vector(by(bt[,1],group,mean,na.rm=TRUE))
      diag(Lambda.hat)<-rep((summary(fit)$sigma)^2,3)
    }
  }
  ##################################################################
  
  ##################################################################
  # max.itr=0: two-stage approach
  # max.itr>0: full likelihood (h-likelihood) approach
  J1<-cbind(diag(rep(1,3*p+1)),matrix(0,3*p+1,3*p+1),matrix(0,3*p+1,1))
  J2<-cbind(matrix(0,3*p+1,3*p+1),diag(rep(1,3*p+1)),matrix(0,3*p+1,1))
  J3<-matrix(c(rep(0,6*p+2),1),nrow=1)
  J<-rbind(c(1,rep(0,3*p+3*p+1+1)),c(rep(0,6*p+2),1),c(rep(0,3*p+1),1,rep(0,3*p+1)))
  
  diff<-100
  s<-0
  while(s<max.itr&diff>=tol)
  {
    bi.new<-matrix(NA,N,3)
    theta.new<-matrix(NA,N,6*p+3)
    for(i in 1:N)
    {
      dd<-dat[[i]]
      
      Zx<-apply(matrix(1:p,ncol=1),1,function(x){return(dd$Z[(p+1-x):(n[i,1]-x)])})
      Mx<-apply(matrix(1:p,ncol=1),1,function(x){return(dd$M[(p+1-x):(n[i,1]-x)])})
      Rx<-apply(matrix(1:p,ncol=1),1,function(x){return(dd$R[(p+1-x):(n[i,1]-x)])})
      
      Zy<-matrix(dd$Z[-(1:p)],ncol=1)
      My<-matrix(dd$M[-(1:p)],ncol=1)
      Ry<-matrix(dd$R[-(1:p)],ncol=1)
      
      X<-cbind(Zy,Zx,Mx,Rx)
      
      kappa<-delta*sqrt(sigma2.hat[i]/sigma1.hat[i])
      
      S1<-t(X%*%J1)%*%(X%*%J1)/sigma1.hat[i]+t(J)%*%solve(Lambda.hat)%*%J+
        t(My%*%J3+X%*%J2-kappa*X%*%J1)%*%(My%*%J3+X%*%J2-kappa*X%*%J1)/(sigma2.hat[i]*(1-delta^2))
      S2<-t(X%*%J1)%*%My/sigma1.hat[i]+t(My%*%J3+X%*%J2-kappa*X%*%J1)%*%(Ry-kappa*My)/(sigma2.hat[i]*(1-delta^2))+
        t(J)%*%solve(Lambda.hat)%*%b.hat
      theta.new[i,]<-solve(S1)%*%S2
      
      if(Sigma.update==TRUE)
      {
        theta1<-theta.new[i,1:(3*p+1)]
        theta2<-theta.new[i,(3*p+2):(6*p+2)]
        
        e1<-My-X%*%theta1
        e2<-Ry-My*theta.new[i,6*p+3]-X%*%theta2
        
        S<-matrix(NA,2,2)
        S[1,1]<-t(e1)%*%e1
        S[1,2]=S[2,1]<-t(e1)%*%e2
        S[2,2]<-t(e2)%*%e2
        
        sigma1.hat[i,1]<-(S[1,1]-delta*S[1,2]*sqrt(S[1,1]/S[2,2]))/((n[i,1]-p)*(1-delta^2))
        sigma2.hat[i,1]<-(S[2,2]-delta*S[1,2]*sqrt(S[2,2]/S[1,1]))/((n[i,1]-p)*(1-delta^2))
      }
      
      bi.new[i,]<-theta.new[i,c(1,6*p+3,3*p+2)]
    }
    b.new<-apply(bi.new,2,mean)
    Lambda.new<-matrix(0,3,3)
    Lambda.tmp<-t(bi.new-matrix(rep(b.new,N),ncol=3,byrow=TRUE))%*%(bi.new-matrix(rep(b.new,N),ncol=3,byrow=TRUE))/N
    if(error.var.equal==FALSE)
    {
      if(error.indep==TRUE)
      {
        diag(Lambda.new)<-diag(Lambda.tmp)
      }else
      {
        Lambda.new<-Lambda.tmp
      }
      
      # variance constraint
      if(sum(var.constraint==FALSE)==0)
      {
        # A
        if(Lambda.new[1,1]<Lambda.confint[1,1])
        {
          Lambda.new[1,1]<-Lambda.confint[1,1]
        }else
          if(Lambda.new[1,1]>Lambda.confint[1,2])
          {
            Lambda.new[1,1]<-Lambda.confint[1,2]
          }
        # B
        if(Lambda.new[2,2]<Lambda.confint[2,1])
        {
          Lambda.new[2,2]<-Lambda.confint[2,1]
        }else
          if(Lambda.new[2,2]>Lambda.confint[2,2])
          {
            Lambda.new[2,2]<-Lambda.confint[2,2]
          }
        # C
        if(Lambda.new[3,3]<Lambda.confint[3,1])
        {
          Lambda.new[3,3]<-Lambda.confint[3,1]
        }else
          if(Lambda.new[3,3]>Lambda.confint[3,2])
          {
            Lambda.new[3,3]<-Lambda.confint[3,2]
          }
      }
    }else
    {
      if(error.indep==TRUE)
      {
        lambda2<-t(c(bi.new-matrix(rep(b.new,N),ncol=3,byrow=TRUE)))%*%(c(bi.new-matrix(rep(b.new,N),ncol=3,byrow=TRUE)))/(3*N)
        diag(Lambda.new)<-rep(lambda2,3)
      }else
      {
        warning("This variance structure is not valide! The errors are assumed to be independent.")
        lambda2<-t(c(bi.new-matrix(rep(b.new,N),ncol=3,byrow=TRUE)))%*%(c(bi.new-matrix(rep(b.new,N),ncol=3,byrow=TRUE)))/(3*N)
        diag(Lambda.new)<-rep(lambda2,3)
      }
    }
    
    diff<-max(abs(b.hat-b.new))
    
    b.hat<-b.new
    Lambda.hat<-Lambda.new
    At<-bi.new[,1]
    Bt<-bi.new[,2]
    Ct<-bi.new[,3]
    
    theta1.new<-theta.new[,1:(3*p+1)]
    theta2.new<-theta.new[,(3*p+2):(6*p+2)]
    
    Wt[,seq(2,2*p,by=2),1]<-theta1.new[,(2*p+2):(3*p+1)]
    Wt[,seq(2,2*p,by=2),2]<-theta2.new[,(2*p+2):(3*p+1)]
    Wt[,seq(1,2*p-1,by=2),1]<-theta1.new[,((p+2):(2*p+1))]+theta1.new[,(2*p+2):(3*p+1)]*Bt
    Wt[,seq(1,2*p-1,by=2),2]<-theta2.new[,((p+2):(2*p+1))]+theta2.new[,(2*p+2):(3*p+1)]*Bt
    
    s<-s+1
  }
  ##################################################################
  
  ##################################################################
  # summary results
  HL<-cma.ts.arp.error.lm.h(dat,delta=delta,Ai=At,Bi=Bt,Ci=Ct,Wi=Wt,b=b.hat,Lambda=Lambda.hat,p=p,Sigma.update=Sigma.update)
  if(max.itr==0)
  {
    # re.HL<-HL$h2
    re.HL<-as.numeric(logLik(fit.A))+as.numeric(logLik(fit.B))+as.numeric(logLik(fit.C))
  }else
  {
    re.HL<-HL$h
  }
  
  AB.p<-b.hat[1]*b.hat[2]
  AB.d<-mean(C2t,na.rm=TRUE)-b.hat[3]
  coe.re<-matrix(NA,6,1)
  colnames(coe.re)<-c("Estimate")
  rownames(coe.re)<-c("A","C","B","C2","AB.prod","AB.diff")
  coe.re[,1]<-c(b.hat[1],b.hat[3],b.hat[2],mean(C2t,na.rm=TRUE),AB.p,AB.d)
  sigma.hat<-cbind(sigma1.hat,sigma2.hat)
  colnames(sigma.hat)<-c("E1","E2")
  ##################################################################
  
  re<-list(delta=delta,Coefficients=coe.re,Lambda=Lambda.hat,Sigma=sigma.hat,W=apply(Wt,c(2,3),mean,na.rm=TRUE),HL=re.HL,
           convergence=(s<max.itr|max.itr==0),var.constraint=Lambda.confint)
  
  return(re)
}
