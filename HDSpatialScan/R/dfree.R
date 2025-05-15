########################################################################################################################################################
##' @title Index for the UG scan procedure
##'
##' @description This function returns the index we want to maximize on the set of potential clusters, for each potential cluster and each permutation
##'
##' @param data numeric matrix. Matrix of the data. The rows correspond to the sites (or the individuals) and each column represents a permutation.
##' @param matrix_clusters numeric matrix. Matrix in which each column represents a potential cluster. It is the result of the "clusters" function.
##'
##' @return numeric matrix.
##'
##'
dfree <- function(data, matrix_clusters)
{
  nb_sites <- nrow(data)
  nb_permu <- ncol(data)

  nb_clusters <- ncol(matrix_clusters)

  mean_in <- matrix(sapply(1:nb_clusters, function(cl) colMeans(data[matrix_clusters[,cl]==1,, drop = FALSE])), ncol = nb_clusters)
  mean_out <- matrix(sapply(1:nb_clusters, function(cl) colMeans(data[matrix_clusters[,cl]==0,, drop = FALSE])), ncol = nb_clusters)

  stat <- matrix(sapply(1:nb_clusters, function(cl) sqrt(sum(matrix_clusters[,cl]) * (nb_sites - sum(matrix_clusters[,cl])) ) * (mean_in[,cl] - mean_out[,cl])), ncol = nb_clusters)


  return(abs(stat))
}
