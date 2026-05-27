g2l.sampler <-
function(n.simu=length(Y),LP.par,Y,clusters=NULL){
  LP.basis <- eLP.poly(Y,length(LP.par))
  d.val <- approxfun(ecdf(Y)(Y),1+LP.basis%*%LP.par, rule=2,method="constant",f=1 )
  d.max <- max(d.val(Y))
  
  sample_iter<-function(iter,dfun,M,Y){
    #for fixing result, remove afterwards.
    #set.seed(50*iter)
    for(i in 1:1000){
      u1 <- runif(1)
      if(dfun(u1) > runif(1)*M){
        out<-quantile(Y,u1)
        break;
      }else{
        out<-NA
      }
    }
    return(as.numeric(out))
  }
  
  iters<-as.matrix(1:n.simu)  
  
  if(is.null(clusters)){
    out<-sapply(X=iters,FUN=sample_iter,
                dfun=d.val,M=d.max,Y=Y)
  }else{
    out<-parSapply(cl=clusters,X=iters,FUN=sample_iter,
                   dfun=d.val,M=d.max,Y=Y)
  }
  
  
  out<-as.numeric(out)
}
