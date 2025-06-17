design.mat <-
function(X,intercept=TRUE)
{
  N<-dim(X)[1]             # # of subject
  ntp<-dim(X)[2]           # # of time points
  
  # design matrix
  q0<-dim(X)[3]
  if(is.na(q0))
  {
    if(intercept)
    {
      q<-2
      
      W<-array(NA,c(N,ntp,q))
      W[,,1]<-matrix(1,nrow=N,ncol=ntp)
      W[,,2]<-X
    }else
    {
      q<-1
      
      W<-array(NA,c(N,ntp,q))
      W[,,1]<-X
    }
  }else
    if(intercept)
    {
      q<-q0+1
      
      W<-array(NA,c(N,ntp,q))
      W[,,1]<-matrix(1,nrow=N,ncol=ntp)
      W[,,2:q]<-X
    }else
    {
      q<-q0
      W<-X
    }
  
  return(list(q=q,W=W))
}
