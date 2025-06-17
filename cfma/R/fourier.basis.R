fourier.basis <-
function(timeinv=c(0,1),ntp,nbasis=3)
{
  if(nbasis%%2==0)
  {
    nbasis<-nbasis-1
  }
  
  timegrids<-seq(timeinv[1],timeinv[2],length.out=ntp)
  
  r<-timeinv[2]-timeinv[1]
  
  basis<-matrix(NA,ntp,nbasis)
  basis[,1]<-rep(1/sqrt(r),ntp)
  for(j in 1:floor(nbasis/2))
  {
    basis[,j*2]<-sin(j*(2*pi*(timegrids-timeinv[1])/r))*sqrt(2/r)
    basis[,j*2+1]<-cos(j*(2*pi*(timegrids-timeinv[1])/r))*sqrt(2/r)
  }
  
  return(basis)
}
