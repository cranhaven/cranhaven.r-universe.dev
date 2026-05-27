eLP.poly.predict <-
function(X,Tx,X.test,mx){
  X <- as.matrix(X)
  X.test<-matrix(X.test,ncol=ncol(X))
  mi<-rep(1,ncol(X)+1)
  Txapprox=matrix(0,nrow(X.test),ncol(Tx))
  for(i in 1:ncol(X)){
    mi[i+1]<-min(mx[1],length(unique(X[,i]))-1)
    Txi<-Predict.LP.poly(X[,i],as.matrix(Tx[,sum(mi[1:i]):(sum(mi[1:(i+1)])-1)]),X.test[,i])
    Txapprox[,sum(mi[1:i]):(sum(mi[1:(i+1)])-1)]=as.matrix(Txi)
  }
  newdat=as.data.frame(Txapprox)
  colnames(newdat)<-colnames(Tx) 
  return(newdat)
}
