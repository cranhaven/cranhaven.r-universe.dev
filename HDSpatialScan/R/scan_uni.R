################################################################
##' @title UNP scan procedure
##'
##' @description This function computes the UNP (Univariate Nonparametric scan statistic).
##'
##' @param data vector. Vector of the data, each element corresponds to a site (or an individual if the observations are by individuals and not by sites).
##' @param MC numeric. Number of Monte-Carlo permutations to evaluate the statistical significance of the clusters. By default: 999.
##' @param typeI numeric. The desired type I error. A cluster will be evaluated as significant if its associated p-value is less than typeI. By default 0.05.
##' @param initialization list. Initialization for the scan procedure (see \code{\link{InitScan}} for more details).
##' @param permutations matrix. Indices of permutations of the data.
##'
##'
##' @return An object of class ResScanOutputUni.
##'
##' @references Inkyung Jung and Ho Jin Cho (2015). A Nonparametric Spatial Scan Statistic for Continuous Data. International Journal of Health Geographics, 14.
##'
##'
UNP <- function(data, MC = 999, typeI = 0.05, initialization, permutations){

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

  if(is.vector(data)){
    if(nrow(matrix_clusters)!=length(data)){
      stop("The number of sites must be the same in the matrix of clusters and in the data")
    }
    cat("Computation of the scan statistic \n")
    rank_data <- rank(data, ties.method = "average")
    index <- wmw_uni(matrix(rank_data, ncol = 1), matrix_clusters)
    cat("---Done--- \n")
    cat("Estimation of the statistical significance \n")
    nb_clusters <- ncol(matrix_clusters)
    nb_sites <- nrow(matrix_clusters)
    generation_signif <- sapply(1:MC, function(g) rank_data[permutations[g,]])

    results <- wmw_uni(matrix(generation_signif, ncol = MC), matrix_clusters)

    stat_MC <- rowMaxs(results)
    pvals <- sapply(1:nb_clusters, function(j) (length(which(stat_MC >= index[1,j]))+1)/(MC+1))
    cat("---Done--- \n")
    cat("Finalization \n")
    index_clusters_temp <- which(pvals <= typeI)

    finalization <- FinScan(index_clusters_temp, index, filtering_post, type_minimaxi_post, mini_post, maxi_post, nb_sites, matrix_clusters, radius, areas, centres, pvals)


  }else{
    stop("The data must be a vector")
  }

  return(ResScanOutputUni(sites_clusters = finalization$sites_clusters, pval_clusters = finalization$pval_clusters, centres_clusters = finalization$centres_clusters, radius_clusters = finalization$radius_clusters, areas_clusters = finalization$areas_clusters, system = system, sites_coord = sites_coord, data = data, method = "UNP"))

}


################################################################
##' @title UG scan procedure
##'
##' @description This function computes the UG (Univariate Gaussian scan statistic).
##'
##' @param data vector. Vector of the data, each element corresponds to a site (or an individual if the observations are by individuals and not by sites).
##' @param MC numeric. Number of Monte-Carlo permutations to evaluate the statistical significance of the clusters. By default: 999.
##' @param typeI numeric. The desired type I error. A cluster will be evaluated as significant if its associated p-value is less than typeI. By default 0.05.
##' @param initialization list. Initialization for the scan procedure (see \code{\link{InitScan}} for more details).
##' @param permutations matrix. Indices of permutations of the data.
##'
##' @return An object of class ResScanOutputUni.
##'
##' @references Martin Kulldorff and Lan Huang and Kevin Konty (2009). A Scan Statistic for Continuous Data Based on the Normal Probability Model. International Journal of Health Geographics, 8 (58).
##'
##'
UG <- function(data, MC = 999, typeI = 0.05, initialization, permutations){

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


  if(is.vector(data)){
    if(nrow(matrix_clusters)!=length(data)){
      stop("The number of sites must be the same in the matrix of clusters and in the data")
    }
    cat("Computation of the scan statistic \n")
    index <- dfree(matrix(data, ncol = 1), matrix_clusters)
    cat("---Done--- \n")
    cat("Estimation of the statistical significance \n")
    nb_clusters <- ncol(matrix_clusters)
    nb_sites <- nrow(matrix_clusters)
    generation_signif <- sapply(1:MC, function(g) data[permutations[g,]])

    results <- dfree(matrix(generation_signif, ncol = MC), matrix_clusters)

    stat_MC <- rowMaxs(results)
    pvals <- sapply(1:nb_clusters, function(j) (length(which(stat_MC >= index[1,j]))+1)/(MC+1))
    cat("---Done--- \n")
    cat("Finalization \n")
    index_clusters_temp <- which(pvals <= typeI)

    finalization <- FinScan(index_clusters_temp, index, filtering_post, type_minimaxi_post, mini_post, maxi_post, nb_sites, matrix_clusters, radius, areas, centres, pvals)


  }else{
    stop("The data must be a vector")
  }

  return(ResScanOutputUni(sites_clusters = finalization$sites_clusters, pval_clusters = finalization$pval_clusters, centres_clusters = finalization$centres_clusters, radius_clusters = finalization$radius_clusters, areas_clusters = finalization$areas_clusters, system = system, sites_coord = sites_coord, data = data, method = "UG"))


}
