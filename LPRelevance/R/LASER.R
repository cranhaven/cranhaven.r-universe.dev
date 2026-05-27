LASER <-
function( X,z, X.target, m=c(4,6), nsample=length(z), lp.reg.method='lm',
                  coef.smooth='BIC', centering=TRUE,parallel=FALSE,...){
  extraparms<-list(...)
  if(is.null(extraparms$k) & lp.reg.method=='knn'){
    extraparms$k<-sqrt(length(z))
  }


  X<-as.matrix(X)
  X.target<-matrix(X.target,1,ncol(X))
  n<-length(z)
  zm.target<-0
  zmean<-rep(0,length(z))
  if(centering==FALSE){
    y<-z
  }else{
    Tx<-eLP.poly(X,m[1])
    centerproc<-z.lp.center(X,Tx,z,lp.reg.method,X.target,m,extraparms)
    y<-centerproc$y
    z.mu.test<-centerproc$z.mu.test
    zmean<-centerproc$zmean
    zm.target<-as.numeric(z.mu.test)
  }


  Lcoef<-LPcden(X,y,m,X.test=X.target,method=lp.reg.method,lp.smooth=coef.smooth,k=extraparms$k)
  if(sum(abs(Lcoef))==0){
    y.sample<-y
  }else{
    if(parallel==TRUE){
      numCores<-round(detectCores()/2)
      cl<-makeCluster(numCores)
    }else{
      cl<-NULL
    }
    y.sample<-g2l.sampler(nsample,LP.par=t(Lcoef),Y=y,clusters=cl)
    if(parallel==TRUE){
      stopCluster(cl)
    }
  }
  out<-list(data=y.sample+as.numeric(zm.target)[1],LPcoef=Lcoef)
  return(out)
}
