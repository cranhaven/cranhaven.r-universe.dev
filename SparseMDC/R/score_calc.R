#' Score calculator
#'
#' Calculates the score for each combination of cluster assignments
#' and center values. This function runs inside \code{sparse_mdc}.
#'
#' @param pdat list with D entries, each entry contains data d, p * n matrix.
#' This data should be centered and log-transformed.
#' @param clusters List containig cluster assignments for each dimension
#' as entries.
#' @param mu list with D entries, each entry contains centers for data d,
#' p*k matrix.
#' @param lambda1 Penalty parameter for mean size.
#' @param lambda2 Penalty parameter for mean difference.
#' @param nclust Number of clusters in the data.
#' @param delta Small term to ensure existance of solution, default is
#'  0.0000001.
#' @param dim Total number of conditions, D.
#'
#' @return The caluculated score.
#'
#' @export
score_calc <- function(pdat, clusters, mu, lambda1, lambda2, nclust, delta, dim){
  mean_penalty <- mean_diff_penalty <- dist_score <- rep(0, nclust)
  nd_vec <- rep(NA, dim)
  # Calculate Distance score
  for(d in 1:dim){
    dist.mat <- matrix(NA, nrow=ncol(pdat[[d]]), ncol=nclust)
    nd_vec[d] <- nrow(dist.mat)
    for (k in 1 : nclust)  # For each cluster do:
    {
      # Calculate distance between each sample and the cluster centers
      dist.mat[, k] <- colSums(((pdat[[d]] - mu[[d]][,k]) ^ 2) * (0.5))
      dist_score[k] <- dist_score[k] + sum(dist.mat[which(clusters[[d]] == k) ,k])
    }
  }

  # Calculate mean value penalty
  for(k in 1:nclust){
    nk_vec <- rep(NA, dim)
    for(d in 1:dim){
      nk_vec[d] <- sum(clusters[[d]] == k)
    }
    pens <- pen_calculator(lambda1 = lambda1, lambda2 = lambda2, nk = nk_vec,
                           delta = delta) # Calculate penalty terms
    pen1 <- pens[[1]] # Extract penalty terms
    pen2 <- pens[[2]] # Extract penalty terms
    for(d in 1:dim){
      mean_penalty[k] <- mean_penalty[k] + (sum(abs(mu[[d]][,k])) * pen1[d])
    }
    for(d in 1:(dim-1)){
      mean_diff_penalty[k] <- mean_diff_penalty[k] + (sum(abs(mu[[d]][,k] - mu[[d+1]][,k])) * pen2[d])
    }
  }
  score <- (sum(dist_score)) + sum(mean_penalty) + sum(mean_diff_penalty)
  return(list(score=score, score_break = c(sum(dist_score),
                                           sum(mean_penalty), sum(mean_diff_penalty))))
}
