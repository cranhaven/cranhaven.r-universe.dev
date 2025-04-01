creanchi<-function(li,ls,nin,v,l){

  d<-ls-li
  x<-matrix(0,nin,1)
  y<-matrix(0,nin,1)
  x[1]<-li
  nr<-nin-1
  amp<-d/nr
  y[1]<-chis(x[1],v,l)*amp

  for (j in 1:nr){
    x[j+1]<-x[j]+amp
    y[j+1]<-chis(x[j+1],v,l)*amp
  }

  tmp <- sum(y)
  y <- (1/tmp) * y
  nodos<-cbind(x,y)

}
