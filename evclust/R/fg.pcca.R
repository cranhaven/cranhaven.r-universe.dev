fg.pcca<-function(l,Xi,Xj,y,C,beta){
  # Function called by pcca (internal function)
  d<-ncol(Xi)
  d1<-length(l)/d
  nc<-length(y)
  L<-matrix(l,d1,d)
  D2<-colSums((L%*%t(Xi-Xj))^2)
  z<-beta*y*(D2-1)
  fun<-sum(log(1+exp(z)))/beta
  A<-apply(C,1,function(Cn) L%*%Cn)
  grad<-2*rowSums(matrix(y/(1+exp(-z)),d*d1,nc,byrow=TRUE)*A)
  return(list(fun=fun,grad=grad))
}

fonc.pcca<-function(l,Xi,Xj,y,C,beta){
  # Function useful to test pcca
  d<-ncol(Xi)
  d1<-length(l)/d
  L<-matrix(l,d1,d)
  D2<-colSums((L%*%t(Xi-Xj))^2)
  z<-1+exp(beta*y*(D2-1))
  fun<-sum(log(z)/beta)
}