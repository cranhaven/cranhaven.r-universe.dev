################################################################
##' @title MNP scan procedure
##'
##' @description This function computes the MNP (Multivariate Nonparametric scan statistic).
##'
##' @param data matrix. Matrix of the data, the rows correspond to the sites (or the individuals if the observations are by individuals and not by sites) and each column represents a variable.
##' @param MC numeric. Number of Monte-Carlo permutations to evaluate the statistical significance of the clusters. By default: 999.
##' @param typeI numeric. The desired type I error. A cluster will be evaluated as significant if its associated p-value is less than typeI. By default 0.05.
##' @param nbCPU numeric. Number of CPU. If nbCPU > 1 parallelization is done. By default: 1.
##' @param variable_names character. Names of the variables. By default NULL
##' @param initialization list. Initialization for the scan procedure (see \code{\link{InitScan}} for more details).
##' @param permutations matrix. Indices of permutations of the data.
##'
##'
##' @return An object of class ResScanOutputMulti.
##'
##' @references Lionel Cucala and Michaël Genin and Florent Occelli and Julien Soula (2019). A Multivariate Nonparametric Scan Statistic for Spatial Data. Spatial statistics, 29, 1-14.
##'
##'
MNP <- function(data, MC = 999, typeI = 0.05, nbCPU = 1, variable_names = NULL, initialization, permutations){

  filtering_post = initialization$filtering_post
  matrix_clusters = initialization$matrix_clusters
  centres = initialization$centres
  radius = initialization$radius
  areas = initialization$areas
  sites_coord = initialization$sites_coord
  system = initialization$system
  mini_post = initialization$mini_post
  maxi_post = initialization$maxi_post
  type_minimaxi_post = initialization$type_minimaxi_post

  if(is(data, "matrix")){
    if(nrow(matrix_clusters)!=nrow(data)){
      stop("The number of sites must be the same in the matrix of clusters and in the data")
    }

    if(ncol(data)==1){
      stop("There must be at least two variables")
    }

    if(is.null(variable_names) == FALSE){
      if(is(variable_names, "character") == FALSE){
        stop("variable_names must be character")
      }
      if(length(variable_names) != ncol(data)){
        stop("The number of elements in variable_names must be the number of columns in data")
      }
    }
    cat("Computation of the scan statistic \n")
    rank_data <- SpatialNP::spatial.rank(data, shape = TRUE)
    index <- multi_WMW(rank_data, matrix_clusters)
    cat("---Done--- \n")
    cat("Estimation of the statistical significance \n")
    nb_clusters <- ncol(matrix_clusters)
    nb_sites <- nrow(matrix_clusters)

    num_cores <- detectCores()

    if(nbCPU <=1 | num_cores<2){
      results <- pblapply(1:MC, function(i) multi_WMW(rank_data[permutations[i,],], matrix_clusters))
    }else{
      if(num_cores < nbCPU){
        nbCPU <- num_cores
      }

      if(.Platform$OS.type == "windows"){
        cl <- makeCluster(nbCPU)
        results <- pblapply(1:MC, function(i) multi_WMW(rank_data[permutations[i,],], matrix_clusters), cl = cl)
        stopCluster(cl)

      }else{
        results <- pblapply(1:MC, function(i) multi_WMW(rank_data[permutations[i,],], matrix_clusters), cl = nbCPU)
      }

    }
    results <- matrix(unlist(results), ncol = nb_clusters, byrow = TRUE)
    stat_MC <- rowMaxs(results)
    pvals <- sapply(1:nb_clusters, function(j) (length(which(stat_MC >= index[j]))+1)/(MC+1))
    cat("---Done--- \n")
    cat("Finalization \n")
    index_clusters_temp <- which(pvals <= typeI)

    finalization <- FinScan(index_clusters_temp, index, filtering_post, type_minimaxi_post, mini_post, maxi_post, nb_sites, matrix_clusters, radius, areas, centres, pvals)

  }else{
    stop("The data must be a matrix")
  }

  return(ResScanOutputMulti(sites_clusters = finalization$sites_clusters, pval_clusters = finalization$pval_clusters, centres_clusters = finalization$centres_clusters, radius_clusters = finalization$radius_clusters, areas_clusters = finalization$areas_clusters, system = system, variable_names = variable_names, sites_coord = sites_coord, data = data, method = "MNP"))

}


################################################################
##' @title MG scan procedure
##'
##' @description This function computes the MG (Multivariate Gaussian scan statistic).
##'
##' @param data matrix. Matrix of the data, the rows correspond to the sites (or the individuals if the observations are by individuals and not by sites) and each column represents a variable.
##' @param MC numeric. Number of Monte-Carlo permutations to evaluate the statistical significance of the clusters. By default: 999.
##' @param typeI numeric. The desired type I error. A cluster will be evaluated as significant if its associated p-value is less than typeI. By default 0.05.
##' @param nbCPU numeric. Number of CPU. If nbCPU > 1 parallelization is done. By default: 1.
##' @param variable_names character. Names of the variables. By default NULL.
##' @param initialization list. Initialization for the scan procedure (see \code{\link{InitScan}} for more details).
##' @param permutations matrix. Indices of permutations of the data.
##'
##'
##'
##' @return An object of class ResScanOutputMulti.
##'
##' @references Lionel Cucala and Michaël Genin and Caroline Lanier and Florent Occelli (2017). A Multivariate Gaussian Scan Statistic for Spatial Data. Spatial Statistics, 21, 66-74.
##'
##'
MG <- function(data, MC = 999, typeI = 0.05, nbCPU = 1, variable_names = NULL, initialization, permutations){

  filtering_post = initialization$filtering_post
  matrix_clusters = initialization$matrix_clusters
  centres = initialization$centres
  radius = initialization$radius
  areas = initialization$areas
  sites_coord = initialization$sites_coord
  system = initialization$system
  mini_post = initialization$mini_post
  maxi_post = initialization$maxi_post
  type_minimaxi_post = initialization$type_minimaxi_post

  if(is(data, "matrix")){
    if(nrow(matrix_clusters)!=nrow(data)){
      stop("The number of sites must be the same in the matrix of clusters and in the data")
    }

    if(ncol(data)==1){
      stop("There must be at least two variables")
    }

    if(is.null(variable_names) == FALSE){
      if(is(variable_names, "character") == FALSE){
        stop("variable_names must be character")
      }
      if(length(variable_names) != ncol(data)){
        stop("The number of elements in variable_names must be the number of columns in data")
      }
    }

    cat("Computation of the scan statistic \n")
    index <- multi_gaussian(data, matrix_clusters)
    cat("---Done--- \n")
    cat("Estimation of the statistical significance \n")
    nb_clusters <- ncol(matrix_clusters)
    nb_sites <- nrow(matrix_clusters)

    num_cores <- detectCores()

    if(nbCPU <=1 | num_cores <2){
      results <- pblapply(1:MC, function(i) multi_gaussian(data[permutations[i,],], matrix_clusters))
    }else{

      if(num_cores < nbCPU){
        nbCPU <- num_cores
      }

      if(.Platform$OS.type == "windows"){
        cl <- makeCluster(nbCPU)
        results <- pblapply(1:MC, function(i) multi_gaussian(data[permutations[i,],], matrix_clusters),cl=cl)
        stopCluster(cl)

      }else{
        results <- pblapply(1:MC, function(i) multi_gaussian(data[permutations[i,],], matrix_clusters), cl = nbCPU)
      }

    }
    results <- matrix(unlist(results), ncol = nb_clusters, byrow = TRUE)
    stat_MC <- rowMins(results)
    pvals <- sapply(1:nb_clusters, function(j) (length(which(stat_MC <= index[j]))+1)/(MC+1))
    cat("---Done--- \n")
    cat("Finalization \n")
    index_clusters_temp <- which(pvals <= typeI)

    finalization <- FinScan(index_clusters_temp, index, filtering_post, type_minimaxi_post, mini_post, maxi_post, nb_sites, matrix_clusters, radius, areas, centres, pvals, maximize = FALSE)


  }else{
    stop("The data must be a matrix")
  }

  return(ResScanOutputMulti(sites_clusters = finalization$sites_clusters, pval_clusters = finalization$pval_clusters, centres_clusters = finalization$centres_clusters, radius_clusters = finalization$radius_clusters, areas_clusters = finalization$areas_clusters, system = system, variable_names = variable_names, sites_coord = sites_coord, data = data, method = "MG"))


}
