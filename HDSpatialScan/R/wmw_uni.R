########################################################################################################################################################
##' @title Index for the UNP scan procedure
##'
##' @description This function returns the index we want to maximize on the set of potential clusters, for each potential cluster, and each permutation
##'
##' @param rank_data matrix. Matrix of the ranks of the data for all permutations. Each column corresponds to a permutation and each row corresponds to a site or an individual.
##' @param matrix_clusters numeric matrix. Matrix in which each column represents a potential cluster. It is the result of the "clusters" function.
##'
##' @return numeric matrix.
##'
##'
wmw_uni <- function(rank_data, matrix_clusters){
  nb_permu <- ncol(rank_data)
  nb_clusters <- ncol(matrix_clusters)

  W <- matrix(sapply(1:nb_clusters, function(cl) colSums(rank_data[which(matrix_clusters[,cl]==1),,drop = FALSE])), ncol = nb_clusters)
  stats_wmw <- matrix(sapply(1:nb_clusters, function(cl) (W[,cl] - sum(matrix_clusters[,cl])*(nrow(rank_data)+1)/2)/sqrt( sum(matrix_clusters[,cl])*(nrow(rank_data) - sum(matrix_clusters[,cl])) *(nrow(rank_data)+1) /12 )), ncol = nb_clusters)

  stat <- abs(stats_wmw)

  return(stat)

}
