W.Gen <-
function(X,k,c.poly=0.5){
#k-terms used in GLP similarity calculation, integer value
#q-quantile for determining sigma in gauss distance

  out<-list()
###LP Transform of data
  Tmat<-apply(X,2,FUN="LPT",k=k)
  R<-apcluster::linKernel(Tmat,normalize=TRUE)   #polynomial kernel
  W<-as.matrix((c.poly+R)^2)
  diag(W)<-0
  
 #output both similarity matrix W and LPT matrix Tmat
  out$W<-W
  out$LPT<-Tmat
 
  return(out)
}
