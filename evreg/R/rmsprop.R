# rmsprop stochastic gradient descent algorithm
# Used by ENNreg
rmsprop<- function(psi0,verbose,options,X,y,K,eps,lambda,xi,rho,nu,optimProto,opt.rmsprop){
  go_on<-TRUE
  N<-length(psi0)
  n<-nrow(X)
  r<-rep(0,N)
  error.best<-Inf
  Error<-NULL
  t<-0
  nbatch<-round(n/opt.rmsprop$batch_size)
  psi<-psi0
  while(go_on){  # MAIN LOOP
    t<-t+1
    batch <- sample(1:nbatch,n,replace=TRUE)
    error<-0
    for(k in 1:nbatch){ # Loop on mini-batches
      ii<-which(batch==k)
      fg<-foncgrad_RFS(psi,as.matrix(X[ii,]),y[ii],K,eps,lambda,xi,rho,nu,optimProto)
      error<-error+fg$fun
      #    RMSprop
      r<-opt.rmsprop$rho*r+(1-opt.rmsprop$rho)*fg$grad*fg$grad
      psi<-psi -(opt.rmsprop$epsi/(opt.rmsprop$delta+sqrt(r)))*fg$grad
    } # end for
    error<- error/nbatch
    if(error<error.best){
      psi.best<-psi
      t.best<-t
      error.best<-error
    }
    if((t>options$maxiter) | (t-t.best > opt.rmsprop$Dtmax)) go_on<-FALSE
    Error<-c(Error,error)
    if(verbose & (t-1)%%options$print==0) cat("iter =",t,'loss =',error,"\n")
  } # end while
  return(list(par=psi.best,Error=Error,value=error.best))
}
