########################################################################################################################################################
##' @title Index for the MDFFSS scan procedure
##'
##' @description This function returns the index we want to maximize on the set of potential clusters, for each potential cluster
##'
##' @param data List. List of the data, each element of the list corresponds to a site (or an individual), for each element each row corresponds to a variable and each column represents an observation time.
##' @param matrix_clusters numeric matrix. Matrix in which each column represents a potential cluster. It is the result of the "clusters" function.
##'
##'
##' @return numeric vector.
##'
##'
dfree_index_multi <- function(data, matrix_clusters){
  nb_clusters <- ncol(matrix_clusters)
  nb_processus <- length(data)

  nb_times <- ncol(data[[1]])

  nb_var <- nrow(data[[1]])
  data_transp <- list()

  for(time in 1:nb_times){
    data_transp[[time]] <- matrix(nrow = nb_var, ncol = nb_processus)
    for(process in 1:nb_processus){
      data_transp[[time]][,process] <- data[[process]][,time]
    }
  }
  temp <- pointwise_mtest_cpp(data_transp, matrix_clusters)
  return(temp$stat2[,1])
}
