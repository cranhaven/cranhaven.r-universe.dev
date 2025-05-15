########################################################################################################################################################
##' @title Index for the URBFSS scan procedure
##'
##' @description This function returns the index we want to maximize on the set of potential clusters, for each potential cluster
##'
##' @param rank_data matrix. Matrix of the ranks of the data for each time. Each column corresponds to an observation time and each row corresponds to a site or an individual.
##' @param matrix_clusters numeric matrix. Matrix in which each column represents a potential cluster. It is the result of the "clusters" function.
##'
##' @return numeric vector.
##'
##'
pointwise_wmw_uni <- function(rank_data, matrix_clusters){
  nb_times <- ncol(rank_data)
  nb_clusters <- ncol(matrix_clusters)

  Wt <- matrix(sapply(1:nb_clusters, function(cl) colSums(rank_data[which(matrix_clusters[,cl]==1),,drop = FALSE])), ncol = nb_clusters)
  stats_wmw <- matrix(sapply(1:nb_clusters, function(cl) (Wt[,cl] - sum(matrix_clusters[,cl])*(nrow(rank_data)+1)/2)/sqrt( sum(matrix_clusters[,cl])*(nrow(rank_data) - sum(matrix_clusters[,cl])) *(nrow(rank_data)+1) /12 )), ncol = nb_clusters)

  stat <- colMaxs(abs(stats_wmw))

  return(stat)

}
