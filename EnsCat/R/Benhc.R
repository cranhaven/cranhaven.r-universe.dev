#########################
Bensemblecluobse<-function(x,En=150){
  Len<-dim(x)
  clusterO<-Len[2]
  b<-1
  knm<-ifelse(Len[1]>100, 70, Len[1]/2)
  knm<-sqrt(Len[1])
  kn<- sample(c(2,knm),1)
  dd<-HCALB(x,kn)
  RE<-dd
  while(b<En){
    kn<-sample(c(2:knm),1)
    sa<-sort(unique(sample(Len[1],Len[1],replace=TRUE)))
    dd<-HCALB(x[sa,],kn)
    dd2<-rep(NA,Len[1])
    dd2[sa]<-dd
    RE<-rbind(RE,dd2)
    b<-b+1
  }
  return(RE)
}

`Benhc` <-
  function(x,En){
    RE2<-Bensemblecluobse(x,En)
    REDIST<-as.dist(hammingD(t(RE2)))
  }



