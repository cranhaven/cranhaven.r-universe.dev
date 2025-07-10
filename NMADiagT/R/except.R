except=function(index,K,nstu){
  list_l<-list()
  for(i in 1:K){
    list_k<-list()
    for(j in 1:nstu){
      if(index[j,i]==2) list_k<-c(list_k,j)
    }
    list_l[[i]]<-list_k
  }
  return(list_l)
}