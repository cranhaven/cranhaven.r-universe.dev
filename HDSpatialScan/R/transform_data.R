########################################################################################################################################################
##' @title Computation of the multivariate functional ranks
##'
##' @description This function computes the multivariate ranks of the data for each observation time
##'
##' @param data List. List of the data, each element of the list corresponds to a site (or an individual), each row corresponds to a variable and each column represents an observation time.
##'
##'
##' @return List
##'
##'
transform_data <- function(data){
  data_transform <- list()
  nb_sites <- length(data)
  nb_var <- nrow(data[[1]])
  nb_times <- ncol(data[[1]])

  for(time in 1:nb_times){
    data_transform[[time]] <- matrix(ncol = nb_var, nrow = nb_sites)
    for(indiv in 1:nb_sites){
      data_transform[[time]][indiv,] <- data[[indiv]][,time]
    }
  }
  for(time in 1:nb_times){
    data_transform[[time]] <- SpatialNP::spatial.rank(data_transform[[time]], shape = TRUE)
  }

  return(data_transform)
}
