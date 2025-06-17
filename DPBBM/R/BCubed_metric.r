###########################################################################
# calculate BCubed metric for clustering evaluation
# input : L, C, alpha
#       L - real label of classes
#       C - classification label of samples
#       alpha - F metric parameter
# output: F metric score

BCubed_metric <- function(L, C, alpha){
  snL <- length(L)
  snC <- length(C)
  
  if(snL != snC){
    stop("length of category does not comply with length of cluster")
    geterrmessage()
  }
  
  # define the correctness of the relation between i and j
  Correctness <- matrix(nrow = snL, ncol = snC)
  ncluster <- c()
  ncategory <- c()
  for( i in 1:snL ){
    for( j in 1:snC ){
      
      if((L[i] == L[j]) & (C[i] == C[j])) Correctness[i,j] <- 1
      else Correctness[i,j] <- 0
    }
  }
  
  # define the num of each cluster
  for(i in 1:snC){
    ncluster[i] <- length(which(C == C[i]))
  }
  
  # define the num of each category
  for(i in 1:snL){
    ncategory[i] <- length(which(L[i] == L))
  }
  
  Precision <- mean(rowSums(Correctness) / ncluster)
  Recall <- mean(colSums(Correctness) / ncategory)
  F <- 1/(alpha/Precision+(1-alpha)/Recall)
  
  return(F)
}