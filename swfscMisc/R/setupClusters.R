#' @title Setup Clusters
#' @description Setup parallel clusters for different operating systems.
#' 
#' @param num.cores number of cores for multithreading. 
#'   If \code{NULL}, the number used is set to the 
#'   value of \code{parallel::detectCores() - 1}.
#' @param max.cores maximum number of cores to use.
#' 
#' @return an object of class \code{c("SOCKcluster", "cluster")}.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @export
#' 
setupClusters <- function(num.cores = 1, max.cores = NULL) {
  # setup clusters
  if(is.null(num.cores)) num.cores <- parallel::detectCores() - 1  
  if(is.null(max.cores)) max.cores <- parallel::detectCores() - 1
  if(is.na(max.cores)) max.cores <- 1
  if(max.cores < 1) max.cores <- 1
  num.cores <- min(num.cores, max.cores)
  if(num.cores > 1) {
    if(.Platform$OS.type == "windows") {
      parallel::makePSOCKcluster(num.cores)
    } else {
      parallel::makeForkCluster(num.cores)
    }
  } else NULL
}