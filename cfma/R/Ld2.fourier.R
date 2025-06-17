Ld2.fourier <-
function(timeinv=c(0,1),ntp,nbasis=3)
{
  if(nbasis%%2==0)
  {
    nbasis<-nbasis-1
  }
  
  timegrids<-seq(timeinv[1],timeinv[2],length.out=ntp)
  
  r<-timeinv[2]-timeinv[1]
  
  db<-matrix(NA,ntp,nbasis)
  db[,1]<-rep(0,ntp)
  for(j in 1:floor(nbasis/2))
  {
    db[,j*2]<--sqrt(2/r)*(2*pi*j/r)^2*sin(2*pi*j*(timegrids-timeinv[1])/r)
    db[,j*2+1]<--sqrt(2/r)*(2*pi*j/r)^2*cos(2*pi*j*(timegrids-timeinv[1])/r)
  }
  
  return(db)
}
