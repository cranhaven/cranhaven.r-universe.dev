################################################################
##' @title Return only the detected clusters with no overlapping in their order of detection
##'
##' @description This function allows to return only the detected clusters with no overlapping in their order of detection.
##'
##' @param index_clusters numeric vector. The indices of the detected clusters.
##' @param matrix_clusters numeric matrix. Matrix in which each column represents a potential cluster. A value of 1 indicate that the site (or the individual) is in the cluster, 0 otherwise.
##'
##'
##' @return The detecting clusters with no overlapping, in their order of detection.
##'
##'
non_overlap <- function(index_clusters, matrix_clusters){

  final_index <- c()
  for(cl in index_clusters){
    add <- TRUE
    for(f in final_index){
      if(length(intersect(which(matrix_clusters[,cl]==1), which(matrix_clusters[,f]==1)))>0){
        add <- FALSE
      }
    }
    if(add == TRUE){
      final_index <- c(final_index, cl)
    }
  }

  return(final_index)

}
