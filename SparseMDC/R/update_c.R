#' update_c
#'
#' Assigns samples to clusters. This function runs inside
#'  \code{sparse_mdc}.
#'
#' @param mu list with D entries, each entry contains centers for data d,
#' p*k matrix.
#' @param pdat list with D entries, each entry contains data d, p * n matrix.
#' This data should be centered and log-transformed.
#' @param nclust Total number of clusters.
#' @param lambda1 Calculated penalty parameter for mean size.
#' @param lambda2 Calculated penalty parameter for mean difference.
#' @param dim Total number of conditions, D.
#' @param delta Small term to ensure existance of solution.
#'
#' @return A list with D entries containing cluster assignments for each
#' sample.
#' @export
update_c <- function(mu, pdat, nclust, dim, lambda1, lambda2, delta){
  clus <- vector(dim, mode = "list") # Create list to store clusters
  nd_vec <- rep(NA, dim)
  for( d in 1:dim){
    nd_vec[d] <- ncol(pdat[[d]])
  }
  for(d in 1:dim){ # for each dimension
    # create distance matrix
    dist.mat <- matrix(NA, nrow=ncol(pdat[[d]]), ncol=nclust)
    nd <- ncol(pdat[[d]])
    for (k in 1 : nclust)  # For each cluster do:
    {
      nk_vec <- rep(1, dim)
      #for(d2 in 1:dim){
      #  nk_vec[d2] <- sum(clusters[[d2]] == k)
      #}
      pens <- pen_calculator(lambda1 = lambda1, lambda2 = lambda2, nk = nk_vec,
                             delta = delta) # Calculate penalty terms
      pen1 <- pens[[1]] # Extract penalty terms
      pen2 <- pens[[2]] # Extract penalty terms

      # Calculate distance between each sample and the cluster centers in data
      dist.mat[, k] <-  ( colSums(((((pdat[[d]] - mu[[d]][,k]) ^ 2) * 0.5)))) +
        (((1) * lambda1[d] * sum(abs(mu[[d]][,k]))))
    }
    # Calcualte the minimum distance for each sample and assign.
    clus[[d]] <- apply(dist.mat, 1, which.min)
  }
  # return cluster assignments
  return(clus)
}
