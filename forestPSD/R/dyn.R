dyn<-function(ax){
  age_range=1:(length(ax)-1)
  Vn<-vector(length=length(ax)-1)
  for(i in 1:length(ax)-1){
    Vn[i]<-(ax[i]-ax[i+1])/max(ax[i],ax[i+1])*100
  }
  Vpi<-1/sum(ax[-length(ax)])*sum(Vn*ax[-length(ax)])
  adj.Vpi<-sum(Vn*ax[-length(ax)])/(min(ax)*length(ax)*sum(ax[-length(ax)]))
  Pmax<-1/(length(ax)*min(ax))*100
  return(list(Vn=data.frame(age_range=age_range,Vn=Vn),Vpi=Vpi,adj.Vpi=adj.Vpi,Pmax=Pmax))
}
