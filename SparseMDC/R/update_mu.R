#' update_mu
#'
#' Update the mean/center values for each cluster and dimension.
#'  This function runs inside \code{sparse_mdc}.
#'
#' @param clusters List containig cluster assignments for each dimension
#' as entries.
#' @param pdat list with D entries, each entry contains data d, p * n matrix.
#' This data should be centered and log-transformed.
#' @param nclust Number of clusters in the data.
#' @param dim Total number of conditions, D.
#' @param lambda1 Calculated penalty parameter for mean size.
#' @param lambda2 Calculated penalty parameter for mean difference.
#' @param ngenes The number of genes in the data.
#' @param delta Small term to ensure existance of solution.
#' @return A list containing the center values for the clusters in each dimensions
#'  as entries.
#' @export
update_mu <- function(clusters, pdat, nclust, dim, lambda1, lambda2, ngenes, delta){
  # Create list to store results
  center_store <- replicate(n =dim, expr = {matrix(rep(NA, nclust*ngenes),
                                                   ncol=nclust)},
                            simplify = F)
  for(k in 1:nclust){ # For each cluster
    nk_vec <- nd_vec <- rep(NA, dim) # create vector to store number in each dimension
    # create matrices to store mean and center values
    mean_mat <- matrix(rep(0, dim*ngenes), ncol=dim)
    for(d in 1:dim){ # For each dimension
      clus_ind <- which(clusters[[d]] == k) # Identify samples in cluster k
      nk_vec[d] <- length(clus_ind)  # Calculate number of samples
      nd_vec[d] <- length(clusters[[d]])
      if(nk_vec[d] > 1){ # If the number of samples is greater than 1
        mean_mat[,d] <- rowMeans(pdat[[d]][,clus_ind, drop = FALSE]) # Calculate mean values
      }  else { # If the dimension is empty
        mean_mat[,d] <- 0 # Set mean value to zero
      }
    }
    # Check which terms will be zero
    if (sum(nk_vec) > 0){

      mu_mat <- matrix(rep(0, dim*(ngenes )), ncol=dim)
      EQ  <-  matrix(rep(0, nrow(mean_mat)*ncol(mean_mat)),
                     ncol=ncol(mean_mat)) # Stores equality values
      v <- matrix(rep(0, nrow(mean_mat)*(ncol(mean_mat)-1)),
                  ncol=(ncol(mean_mat)-1)) # Stores relationship values
      pens <- pen_calculator(lambda1 = lambda1, lambda2 = lambda2,
                             nk = nk_vec, delta = delta) # Calculate penalty terms
      pen1 <- pens[[1]] # Extract penalty terms
      pen2 <- pens[[2]] # Extract penalty terms
      # Solve for new value of mu
      mu_mat <- mu_solver(d = dim, mu = mu_mat, v = v, EQ = EQ, dim = dim,
                          x = mean_mat, nk = nk_vec, p1 = pen1, p2 = pen2,
                          delta = delta)
      for(d in 1:dim){
        center_store[[d]][,k] <- mu_mat[,d]
      }
    } else {
      for(d in 1:dim){
        # Extract center value for each dimension
        center_store[[d]][,k] <- 0
      }
    }

  }
  return(center_store)
}
