#'@title BetaCV
#'
#'@description function to calculates the BetaCV.
#'
#'@references
#'
#'@return list
#'
#'@importFrom("stats","dist")
#'
#'@example
#'x <- runif(20,-1,1)
#'y <- runif(20,-1,1)
#'dataset <- cbind(x,y)
#'l <- lsdbc(dataset, 7,3,"euclidean")
#'dmat <- as.matrix(dist(dataset,"euclidean"))
#'betaCV(l$cluster,dmat)
#'
#'@export

betaCV <- function(clust, dist){
  inter_weight <- 0
  intra_weight <- 0
  n <- nrow(dist)

  for (i in 1:n){
    if(clust[i]!=0){
      for (j in 1:n){
        if (clust[i]==clust[j]){
          intra_weight <- intra_weight + dist[i,j]
        } else {
          inter_weight <- inter_weight + dist[i,j]
        }
      }
    }
  }

  intra_weight <- intra_weight/2
  inter_weight <- inter_weight/2

  k <- max(clust)
  ni <- 0
  nj <- 0
  n_intra <- 0
  n_inter <- 0
  for (i in 1:k){
    ni <- length(clust[which(clust==i)])
    n_intra <- n_intra + choose(ni,2)
  }

  for (i in 1:(k-1)){
    for(j in (i+1):k){
      ni <- length(clust[which(clust==i)])
      nj <- length(clust[which(clust==j)])
      n_inter <- n_inter + (ni*nj)
    }
  }

  betaCV <- ((intra_weight/n_intra)/(inter_weight/n_inter))
  return(betaCV)
}
