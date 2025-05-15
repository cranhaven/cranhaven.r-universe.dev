########################################################################################################################################################
##' @title Index for the MRBFSS scan procedure
##'
##' @description This function returns the index we want to maximize on the set of potential clusters, for each potential cluster
##'
##' @param transform_data List. List of the data transformed with the function transform_data, each element of the list corresponds to an observation time. Each row of each element is a site (or an individual), and each column represents a variable.
##' @param matrix_clusters numeric matrix. Matrix in which each column represents a potential cluster. It is the result of the "clusters" function.
##'
##'
##' @return numeric vector.
##'
##'
pointwise_wmw_multi <- function(transform_data, matrix_clusters){
  nb_times <- length(transform_data)

  stats_wmw <- sapply(1:nb_times, function(time) as.vector(mwmw_opti_cpp(rank_data = transform_data[[time]], matrix_clusters = matrix_clusters)))

  stat2 <- rowMaxs(stats_wmw)

  return(stat2)

}
