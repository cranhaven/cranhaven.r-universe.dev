SelectCluster<-function(X,pc){

nr<-NROW(X)
ind<-matrix(rep(1,nr),nrow = nr, ncol = 1)
expand<-matrix(rep(1,nr),nrow = nr, ncol = 1)
for(jj in 1:NCOL(X)){
  ind <- (ind & (X[,jj] >= pc[expand,jj])) &  (X[,jj] <= pc[2*expand,jj])
}
ind<-which(ind)

return(ind)

}
