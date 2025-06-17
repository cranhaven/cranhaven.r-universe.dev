

Owen_value<-function(characteristic_function,union,game=c("profit","cost")){
  
  # Edit information
  U<-union
  cS<-characteristic_function
  gamech<-game
  
  if (gamech=="profit"){cS<-cS*(-1)}
  
  
  n<-log(length(cS),2)
  if (n!=round(n)){
    cS<-c(0,cS)
    n<-log(length(cS),2)
  }
  
  game<-cbind(coalitions(n)[[1]],cS)
  costes<-matrix(0,ncol=2,nrow=length(cS))
  for (i in 1:(2^n-1)){
    for (j in 1:n){
      costes[i+1,1]<-costes[i+1,1]+2^(j-1)*game[i+1,j]
      costes[i+1,2]<-cS[i+1]
    }
  }
  
  p <- permutations(n)
  
  # Choose the available permutations
  index_permutation<-c()
  for (k in 1:nrow(p)){
    for (i in 1:length(U)){
      union_aux<-U[[i]]
      index<-c()
      for (j in 1:length(union_aux)){index[j]<-which(p[k,]==union_aux[j])}
      index_sort<-sort(index)
      for (j in 1:(length(index_sort)-1)){
        if (length(index_sort)>1){
          if ((index_sort[j+1]-index_sort[j])!=1){
            index_permutation<-c(index_permutation,k)
          }
        }
      }
    }
  }
  if(length(index_permutation)>0){p<-p[-index_permutation,]}
  
  # Calculate the marginal contributions
  owen <- c()
  for (i in 1:n) {
    cj <- 0
    for (j in 1:nrow(p)) {
      aux <- numeric()
      pred <- sort(p[j, 1:which(p[j, ] == i) - 1], decreasing = F)
      aux <- sort(union(pred, i), decreasing = F)
      v_pred<-rep(0,n);v_aux<-rep(0,n)
      if (length(pred)>0){v_pred[pred]<-1}
      if (length(aux)>0){v_aux[aux]<-1}
      
      ipred<-0;iaux<-0
      for (k in 1:n){
        ipred<-ipred+2^(k-1)*v_pred[k]
        iaux<- iaux+2^(k-1)*v_aux[k]
      }
      
      cj <- cj + costes[which(costes[,1] == iaux),2] - 
        costes[which(costes[,1] == ipred),2]
    }
    owen[i] <- cj/(nrow(p))
  }
  
  if (gamech=="profit"){owen<-owen*(-1)}
  
  
  owen<-matrix(owen,ncol=n)
  colnames(owen)<-1:n
  rownames(owen)<-" "
  print("Owen Value")
  return(owen)}
