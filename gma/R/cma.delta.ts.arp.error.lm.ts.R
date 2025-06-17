cma.delta.ts.arp.error.lm.ts <-
function(dat,delta=0,p=1,error.indep=FALSE,error.var.equal=FALSE)
{
  N<-length(dat)
  K<-1
  
  ##############################################
  # Estimate A, B, C and C' for each subject
  At=Bt=Ct=C2t<-matrix(NA,N,K)
  sigma1.hat=sigma2.hat<-matrix(NA,N,K)
  Wi<-array(NA,c(N,2*p,2))
  for(i in 1:N)
  {
    dd<-dat[[i]]
    re<-cma.uni.delta.ts.arp.error(dd,delta=delta,p=p,var.asmp=FALSE)
    
    At[i,1]<-re$Coefficients[1,1]
    Ct[i,1]<-re$Coefficients[2,1]
    Bt[i,1]<-re$Coefficients[3,1]
    C2t[i,1]<-re$Coefficients[4,1]
    
    sigma1.hat[i]<-re$Sigma[1,1]
    sigma2.hat[i]<-re$Sigma[2,2]
    
    Wi[i,,]<-re$W
  }
  Wt<-apply(Wi,c(2,3),mean,na.rm=TRUE)
  ##############################################
  
  ##############################################
  #
  Lambda.hat<-matrix(0,3,3)
  colnames(Lambda.hat)=rownames(Lambda.hat)<-c("A","B","C")
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
      
      ll<-as.numeric(logLik(fit.A)+logLik(fit.B)+logLik(fit.C))
    }else
    {
      fit.b<-lm(cbind(At,Bt,Ct)~1)
      aov.b<-Anova(fit.b)
      b.hat<-as.vector(coef(fit.b))
      Lambda.hat<-aov.b$SSPE/aov.b$error.df
      res<-cbind(At-b.hat[1],Bt-b.hat[1],Ct-b.hat[3])
      
      ll<-log(2*pi)*(-3*N/2)-log(det(Lambda.hat))/2-sum(diag(solve(Lambda.hat)%*%t(res)%*%res))/2
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
      
      ll<-as.numeric(logLik(fit))
    }else
    {
      warning("This variance structure is not valid! The errors are assumed to be independent.")
      bt<-rbind(At,Bt,Ct)
      group<-c(rep("A",length(At)),rep("B",length(Bt)),rep("C",length(Ct)))
      fit<-lm(bt~group)
      b.hat<-as.vector(by(bt[,1],group,mean,na.rm=TRUE))
      diag(Lambda.hat)<-rep((summary(fit)$sigma)^2,3)
      
      ll<-as.numeric(logLik(fit))
    }
  }
  ##############################################
  
  ##############################################
  # summary results
  AB.p<-b.hat[1]*b.hat[2]
  AB.d<-mean(C2t,na.rm=TRUE)-b.hat[3]
  coe.re<-matrix(NA,6,1)
  colnames(coe.re)<-c("Estimate")
  rownames(coe.re)<-c("A","C","B","C2","AB.prod","AB.diff")
  coe.re[,1]<-c(b.hat[1],b.hat[3],b.hat[2],mean(C2t,na.rm=TRUE),AB.p,AB.d)
  sigma.hat<-cbind(sigma1.hat,sigma2.hat)
  colnames(sigma.hat)<-c("E1","E2")
  ##############################################
  
  re<-list(delta=delta,Coefficients=coe.re,Lambda=Lambda.hat,Sigma=sigma.hat,W=Wt,logLik.lm=ll)
  return(re)
}
