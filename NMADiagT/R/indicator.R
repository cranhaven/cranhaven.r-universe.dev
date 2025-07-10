indicator=function(K,nstu,dat){
  newdat<-dat[c(1:(K+2))]
  dat1<-unique(newdat)
  indicatorm<-matrix(NA, nrow = nstu, ncol = K)
  i=0
  j=0
  for(i in 3:(K+2)){
    for(j in 1:nstu){
      if(dat1[j,i]==1) indicatorm[j,(i-2)]=1
      else indicatorm[j,(i-2)]=2
    }
  }
  return(indicatorm)
}