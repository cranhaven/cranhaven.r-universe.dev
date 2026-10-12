`CTN`<-
  function(x){
    CN4<-function(XX0){
      XX1<-NULL
      XX1[XX0=='a']<-1
      XX1[XX0=='t']<-2
      XX1[XX0=='c']<-3
      XX1[XX0=='g']<-4
      XX1[XX0=='-']<-NA
      XX1[XX0=='0']<-NA
      XX1[XX0=='A']<-1
      XX1[XX0=='T']<-2
      XX1[XX0=='C']<-3
      XX1[XX0=='G']<-4
      return(XX1)
    }
  data0<-matrix(,ncol=length(x[[1]]),nrow=length(x))
  for(i in 1:length(x)){
  data0[i,]<-CN4(x[[i]])
  }
  data0
}

