FMA.concurrent <-
function(Z,M,Y,intercept=TRUE,basis=NULL,Ld2.basis=NULL,basis.type=c("fourier"),nbasis=3,timeinv=c(0,1),timegrids=NULL,lambda.m=0.01,lambda.y=0.01)
{
  N<-nrow(Z)             # # of subject
  ntp<-ncol(Z)           # # of time points
  
  if(is.null(timegrids))
  {
    timegrids<-seq(timeinv[1],timeinv[2],length.out=ntp)
  }
  
  # M model
  fit.m<-FDA.concurrent(Z,M,intercept=intercept,basis=basis,Ld2.basis=Ld2.basis,basis.type=basis.type,nbasis=nbasis,timeinv=timeinv,timegrids=timegrids,lambda=lambda.m)
  # Y model
  Xtmp<-array(NA,c(N,ntp,2))
  Xtmp[,,1]<-Z
  Xtmp[,,2]<-M
  fit.y<-FDA.concurrent(Xtmp,Y,intercept=intercept,basis=basis,Ld2.basis=Ld2.basis,basis.type=basis.type,nbasis=nbasis,timeinv=timeinv,timegrids=timegrids,lambda=lambda.y)
  
  if(intercept)
  {
    coef.inter.m<-fit.m$coefficients[1:ncol(fit.m$basis)]
    coef.inter.y<-fit.y$coefficients[1:ncol(fit.m$basis)]
    curve.inter.m<-fit.m$gamma.curve[1,]
    curve.inter.y<-fit.y$gamma.curve[2,]
    
    coef.alpha<-fit.m$coefficients[-(1:ncol(fit.m$basis))]
    coef.gamma<-fit.y$coefficients[(ncol(fit.y$basis)+1):(2*ncol(fit.y$basis))]
    coef.beta<-fit.y$coefficients[(2*ncol(fit.y$basis)+1):(3*ncol(fit.y$basis))]
    
    curve.alpha<-fit.m$gamma.curve[2,]
    curve.gamma<-fit.y$gamma.curve[2,]
    curve.beta<-fit.y$gamma.curve[3,]
    
    coef.IE<-matrix(coef.alpha*coef.beta,nrow=1)
    rownames(coef.IE)<-"IE"
    colnames(coef.IE)<-paste0("basis",1:ncol(fit.m$basis))
    curve.IE<-curve.alpha*curve.beta
    
    # M model
    coef.m<-rbind(coef.inter.m,coef.alpha)
    rownames(coef.m)<-c("Intercept","Z")
    colnames(coef.m)<-paste0("basis",1:ncol(fit.m$basis))
    curve.m<-rbind(curve.inter.m,curve.alpha)
    rownames(curve.m)<-c("Intercept","Z")
    
    re.m<-list(coefficients=coef.m,curve=curve.m,fitted=fit.m$fitted,lambda=lambda.m)
    
    # Y model
    coef.y<-rbind(coef.inter.y,coef.gamma,coef.beta)
    rownames(coef.y)<-c("Intercept","Z","M")
    colnames(coef.y)<-paste0("basis",1:ncol(fit.y$basis))
    curve.y<-rbind(curve.inter.y,curve.gamma,curve.beta)
    rownames(curve.y)<-c("Intercept","Z","M")
    
    re.y<-list(coefficients=coef.y,curve=curve.y,fitted=fit.y$fitted,lambda=lambda.y)
    
    re.IE<-list(coefficients=coef.IE,curve=curve.IE)
    
    re.DE<-list(coefficients=coef.gamma,curve=curve.gamma)
    
    re<-list(basis=basis,M=re.m,Y=re.y,IE=re.IE,DE=re.DE)
  }else
  {
    coef.alpha<-fit.m$coefficients
    coef.gamma<-fit.y$coefficients[1:ncol(fit.y$basis)]
    coef.beta<-fit.y$coefficients[(ncol(fit.y$basis)+1):(2*ncol(fit.y$basis))]
    
    curve.alpha<-fit.m$gamma.curve[1,]
    curve.gamma<-fit.y$gamma.curve[1,]
    curve.beta<-fit.y$gamma.curve[2,]
    
    coef.IE<-matrix(coef.alpha*coef.beta,nrow=1)
    rownames(coef.IE)<-"IE"
    colnames(coef.IE)<-paste0("basis",1:ncol(fit.m$basis))
    curve.IE<-curve.alpha*curve.beta
    
    # M model
    coef.m<-rbind(coef.alpha)
    rownames(coef.m)<-c("Z")
    colnames(coef.m)<-paste0("basis",1:ncol(fit.m$basis))
    curve.m<-matrix(curve.alpha,nrow=1)
    rownames(curve.m)<-"Z"
    
    re.m<-list(coefficients=coef.m,curve=curve.m,fitted=fit.m$fitted,lambda=lambda.m)
    
    # Y model
    coef.y<-rbind(coef.gamma,coef.beta)
    rownames(coef.y)<-c("Z","M")
    colnames(coef.y)<-paste0("basis",1:ncol(fit.y$basis))
    curve.y<-rbind(curve.gamma,curve.beta)
    rownames(curve.y)<-c("Z","M")
    
    re.y<-list(coefficients=coef.y,curve=curve.y,fitted=fit.y$fitted,lambda=lambda.y)
    
    re.IE<-list(coefficients=coef.IE,curve=curve.IE)
    
    re.DE<-list(coefficients=coef.gamma,curve=curve.gamma)
    
    re<-list(basis=fit.m$basis,M=re.m,Y=re.y,IE=re.IE,DE=re.DE)
  }
  
  return(re)
}
