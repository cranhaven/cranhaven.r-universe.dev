# Computation of credal partition with Hopfield procedure
# Used by EkNNclus

credal.partition <- function(y,alpha,index){
  n<-length(y)
  c<-max(y)
  K<-dim(alpha)[2]
  m<-cbind(matrix(0,n,c),rep(1,n))
  for(i in (1:n)){
    for(j in (1:K)){
      m1<-rep(0,c+1)
      k<-y[index[i,j]]
      m1[k] <- alpha[i,j]
      m1[c+1] <- 1-alpha[i,j]
      m[i,1:c] <- m1[1:c]*(m[i,1:c]+m[i,c+1])+ m1[c+1]*m[i,1:c]
      m[i,c+1]<- m1[c+1]*m[i,c+1]
      m[i,]<-m[i,]/sum(m[i,])
    }
  }
  m<-cbind(rep(0,n),m)
  return(m)
}
