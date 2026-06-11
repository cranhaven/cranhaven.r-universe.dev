LPT <-
function(x,k){ 
  k1<-min(length(unique(x))-1,k)
  if(k1>=k){
    u <- (rank(x,ties.method = c("average")) - .5)/length(x) 
    S.mat <- as.matrix(poly(u ,df=k)) 
    return(as.matrix(scale(S.mat))[,k])
  }else{
    return(as.matrix(rep(0,length(x))))
  }
}
