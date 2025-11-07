lifetable<-function(ax){
  ax<-ax
  lx<-round(ax/ax[1]*1000,0)
  lnlx<-log(lx,base=exp(1))
  dx<-vector(length=length(ax))
  Lx<-vector(length=length(ax))
  Tx<-vector(length=length(ax))
  Sx<-vector(length=length(ax))
  for(i in 1:length(ax)){
    dx[i]=lx[i]-lx[i+1]
    Lx[i]=(lx[i]+lx[i+1])/2
    Sx[i]=lx[i+1]/lx[i]
  }
  for(i in 1:length(ax)){
    Tx[i]=sum(Lx[length(Lx):i],na.rm=TRUE)
  }
  Tx[length(Tx)]<-NA
  qx<-dx/lx
  ex<-Tx/lx
  Kx<-lnlx-c(lnlx[-1],NA)
  return(cbind(Ntable(ax=ax),data.frame(lx=lx,lnlx=lnlx,dx=dx,
                                        qx=qx,Lx=round(Lx,0),Tx=round(Tx,0),ex=ex,Kx=Kx,Sx=Sx)))
}
