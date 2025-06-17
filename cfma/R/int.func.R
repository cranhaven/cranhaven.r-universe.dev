int.func <-
function(x,timeinv=c(0,1),timegrids=NULL)
{
  ntp<-length(x)
  
  if(is.null(timegrids))
  {
    timegrids<-seq(timeinv[1],timeinv[2],length.out=ntp)
  }
  
  if(timeinv[2]<=timeinv[1])
  {
    int<-0
  }else
  {
    if(length(timegrids)==ntp)
    {
      int<-0
      for(i in 1:(ntp-1))
      {
        int<-int+(x[i]+x[i+1])*(timegrids[i+1]-timegrids[i])/2
      }
    }else
    {
      stop("Error!")
    }
  }
  
  return(int)
}
