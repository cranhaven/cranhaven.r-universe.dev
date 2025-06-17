sim.data.single <-
function(Z,Theta,Sigma)
{
  n<-length(Z)
  
  s<-svd(Sigma)
  Sigma.root<-s$u%*%diag(sqrt(s$d))%*%t(s$v)
  
  E<-matrix(rnorm(2*n),nrow=n)%*%Sigma.root
  
  M<-Z*Theta[1,1]+E[,1]
  R<-Z*Theta[1,2]+M*Theta[2,2]+E[,2]
  
  re<-data.frame(Z=Z,M=M,R=R)
  return(re)
}
