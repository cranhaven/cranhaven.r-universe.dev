MaxEndNodes<-function(t,NTrees,defMaxSize,defMaxDepth){
if(NTrees==1){
  if(defMaxSize>0){
    Size<-min(defMaxSize,2^(defMaxDepth-1))
  }else{
    Size<-2^(defMaxDepth-1)
  }
}else{
  if(defMaxSize>0){
    Size<-t
  }else{
    Size<-2^(t-1)
  }
}

return(Size)
}
