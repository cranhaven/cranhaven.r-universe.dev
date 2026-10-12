cluster<-function(data){
  res<-kmeans(data,2)
  if(res$centers[1,1] > res$centers[2,1]){
    l<-which(res$cluster==1)
    res$cluster[res$cluster==2]=1
    res$cluster[l]=2
  }
  meth.recode<-res$cluster-1
  return(meth.recode)
}
