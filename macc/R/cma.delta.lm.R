cma.delta.lm <-
function(dat,delta=0,max.itr=500,tol=1e-4,error.indep=FALSE,error.var.equal=FALSE,Sigma.update=FALSE,
                       var.constraint=FALSE)
{
  N<-length(dat)
  K<-1
  
  ######################################
  # Estimate A, B, C and C' for each subject
  At=Bt=Ct=C2t<-matrix(NA,N,K)
  sigma1=sigma2<-matrix(NA,N,K)
  for(i in 1:N)
  {
    dd<-dat[[i]]
    re<-cma.uni.delta(dd,delta=delta)
    
    At[i,1]<-re$D[1,1]
    Ct[i,1]<-re$D[1,2]
    Bt[i,1]<-re$D[2,2]
    C2t[i,1]<-re$Coefficients[4,1]
    
    sigma1[i]<-re$Sigma[1,1]
    sigma2[i]<-re$Sigma[2,2]
  }
  ######################################
  
  ######################################
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
      intA<-intervals(fit.A)
      intB<-intervals(fit.B)
      intC<-intervals(fit.C)
      Lambda.confint[1,]<-as.matrix(intA[[2]][c(1,3)])^2
      Lambda.confint[2,]<-as.matrix(intB[[2]][c(1,3)])^2
      Lambda.confint[3,]<-as.matrix(intC[[2]][c(1,3)])^2 
    }
  }else
    if(var.constraint)
    {
      Lambda.confint<-matrix(NA,3,2)
      colnames(Lambda.confint)<-c("LB","UB")
      rownames(Lambda.confint)<-c("A","B","C")
      intA<-intervals(fit.A)
      intB<-intervals(fit.B)
      intC<-intervals(fit.C)
      Lambda.confint[1,]<-as.matrix(intA[[2]][c(1,3)])^2
      Lambda.confint[2,]<-as.matrix(intB[[2]][c(1,3)])^2
      Lambda.confint[3,]<-as.matrix(intC[[2]][c(1,3)])^2 
    }else
    {
      Lambda.confint<-NULL
    }
  ######################################
  
  ######################################
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
    if(error.indep)
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
  ######################################
  
  ######################################
  # max.itr=0: the Two-stage approach
  # max.itr>0: h-likelihood approach
  diff<-100
  s<-0
  while(s<max.itr&diff>=tol)
  {
    bi.new<-matrix(NA,N,3)
    for(i in 1:N)
    {
      dd<-dat[[i]]
      Y<-cbind(dd$M,dd$R)
      X<-cbind(dd$Z,dd$M)
      
      if(Sigma.update)
      {
        Theta<-matrix(c(At[i],0,Ct[i],Bt[i]),2,2)
        S<-t(Y-X%*%Theta)%*%(Y-X%*%Theta)
        
        sigma1[i]<-(S[1,1]-delta*S[1,2]*sqrt(S[1,1]/S[2,2]))/(nrow(dd)*(1-delta^2))
        sigma2[i]<-(S[2,2]-delta*S[1,2]*sqrt(S[2,2]/S[1,1]))/(nrow(dd)*(1-delta^2))
      }
      
      P<-matrix(c(-delta*sqrt(sigma2[i]/sigma1[i]),0,0,1,1,0),2,3)
      Q<-c(0,delta*sqrt(sigma2[i]/sigma1[i]))
      V<-matrix(c(1,0,0),nrow=1)
      # Update parameters
      bi.new[i,]<-solve(t(X%*%P)%*%(X%*%P)/(sigma2[i]*(1-delta^2))+t(dd$Z%*%V)%*%(dd$Z%*%V)/sigma1[i]+solve(Lambda.hat))%*%
        (t(X%*%P)%*%(dd$R-X%*%Q)/(sigma2[i]*(1-delta^2))+t(dd$Z%*%V)%*%dd$M/sigma1[i]+solve(Lambda.hat)%*%b.hat)
    }
    b.new<-apply(bi.new,2,mean)
    Lambda.new<-matrix(0,3,3)
    Lambda.tmp<-t(bi.new-matrix(rep(b.new,N),ncol=3,byrow=TRUE))%*%(bi.new-matrix(rep(b.new,N),ncol=3,byrow=TRUE))/N
    if(!error.var.equal)
    {
      if(error.indep)
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
      if(error.indep)
      {
        lambda2<-t(c(bi.new-matrix(rep(b.new,N),ncol=3,byrow=TRUE)))%*%c(bi.new-matrix(rep(b.new,N),ncol=3,byrow=TRUE))/(3*N)
        diag(Lambda.new)<-rep(lambda2,3)
      }else
      {
        warning("This variance structure is not valid! The errors are assumed to be independent.")
        lambda2<-t(c(bi.new-matrix(rep(b.new,N),ncol=3,byrow=TRUE)))%*%c(bi.new-matrix(rep(b.new,N),ncol=3,byrow=TRUE))/(3*N)
        diag(Lambda.new)<-rep(lambda2,3)
      }
    }
    
    
    diff<-max(abs(b.hat-b.new))
    
    b.hat<-b.new
    Lambda.hat<-Lambda.new
    At<-bi.new[,1]
    Bt<-bi.new[,2]
    Ct<-bi.new[,3]
    s<-s+1
    
    # print(c(s,diff))
  }
  ######################################
  
  ######################################
  # summary results
  HL<-cma.lm.h(dat,delta=delta,A.i=At,B.i=Bt,C.i=Ct,b=b.hat,Lambda=Lambda.hat,Sigma.update=Sigma.update)
  if(max.itr==0)
  {
    re.HL<-HL$h2
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
  sigma.hat<-cbind(sigma1,sigma2)
  colnames(sigma.hat)<-c("E1","E2")
  ######################################
  
  re<-list(delta=delta,Coefficients=coe.re,Lambda=Lambda.hat,Sigma=sigma.hat,HL=re.HL,convergence=(s<max.itr|max.itr==0),
           Var.constraint=Lambda.confint)
  
  return(re)
}
