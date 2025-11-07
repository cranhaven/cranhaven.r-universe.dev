Mpre<-function(ax,n=c(2,4,6)){
  M0=ax
  Mi<-list()
  for(i in 1:length(n)){
    Mi[[i]]=round(TTR::SMA(x=ax,n=n[i]),0)
  }
  Mdf<-as.data.frame(do.call(cbind,Mi))
  Mdf<-cbind(Ntable(ax=ax),Mdf)
  names(Mdf)[3:(length(n)+3)]<-paste("M",c(0,n),sep="")
  return(Mdf)
}
