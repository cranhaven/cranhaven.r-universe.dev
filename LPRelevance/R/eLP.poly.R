eLP.poly <-
function(X,m){
  X <- as.matrix(X)
  Xcols<-split(X, rep(1:ncol(X), each = nrow(X)))
  Tcols<-lapply(Xcols,eLP.univar,m)
  colname_list<-c()
  for(i in 1:length(Tcols)){
    mi<-ncol(Tcols[[i]])
    colname_list<-c(colname_list,paste0('X',i,'T',1:mi))
  }
  T0<-do.call(cbind,Tcols)
  colnames(T0)<-colname_list
  return(T0)
}
