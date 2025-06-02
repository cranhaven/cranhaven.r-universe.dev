# Simplification of labels within the Hopfield procedure
# Used by EkNNclus

simplify.labels<- function(y){
  n<-length(y)
  s<-sort(y,index.return=TRUE)
  ys<-s$x
  y1<-vector(length=n)
  y1[s$ix[1]]<-1
  for(i in (2:n)){
    if(ys[i]==ys[i-1]) y1[s$ix[i]]<-y1[s$ix[i-1]] else y1[s$ix[i]]<-y1[s$ix[i-1]]+1
  }
  return(y1)
}
