################################################################
##' @title NPFSS scan procedure (univariate functional or multivariate functional)
##'
##' @description This function computes the NPFSS (Nonparametric Functional scan statistic for multivariate or univariate functional data).
##'
##' @param data list of numeric matrices or a matrix. List of nb_sites (or nb_individuals if the observations are by individuals and not by site) matrices of the data, the rows correspond to the variables and each column represents an observation time (multivariate case) ; or Matrix of the data, the rows correspond to the sites (or to the individuals) and each column represents an observation time (univariate case). The times must be equally spaced and the same for each site/individual.
##' @param MC numeric. Number of Monte-Carlo permutations to evaluate the statistical significance of the clusters. By default: 999.
##' @param typeI numeric. The desired type I error. A cluster will be evaluated as significant if its associated p-value is less than typeI. By default 0.05.
##' @param nbCPU numeric. Number of CPU. If nbCPU > 1 parallelization is done. By default: 1.
##' @param variable_names character. Names of the variables. By default NULL. Ignored if the data is a matrix (univariate functional case).
##' @param times numeric. Times of observation of the data. By default NULL.
##' @param initialization list. Initialization for the scan procedure (see \code{\link{InitScan}} for more details).
##' @param permutations matrix. Indices of permutations of the data.
##'
##'
##' @return An object of class ResScanOutputUniFunct or ResScanOutputMultiFunct depending on the data
##'
##' @references Zaineb Smida and Lionel Cucala and Ali Gannoun and Ghislain Durif (2022). A Wilcoxon-Mann-Whitney spatial scan statistic for functional data. Computational Statistics & Data Analysis, 167.
##'
##'
NPFSS <- function(data, MC = 999, typeI = 0.05, nbCPU = 1, variable_names = NULL, times = NULL, initialization, permutations){

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

  # univariate or multivariate ?
  if(is(data, "list")){
    # multivariate
    if(length(data)!=nrow(matrix_clusters)){
      stop("The data must contain the same number of sites than the matrix of clusters")
    }
    nb_rows <- sapply(1:length(data), function(r) nrow(data[[r]]))
    nb_cols <- sapply(1:length(data), function(r) ncol(data[[r]]))

    if(sum((nb_rows!=nb_rows[1])) + sum((nb_cols!=nb_cols[1])) > 0){
      stop("All matrices of the list must be of the same dimensions")
    }
    if(nb_rows[1]==1){
      stop("There must be at least two variables (rows)")
    }
    if(nb_cols[1]==1){
      stop("There must be at least two observation times (columns)")
    }

    if(is.null(variable_names) == FALSE){
      if(is(variable_names, "character") == FALSE){
        stop("variable_names must be character")
      }
      if(length(variable_names) != nrow(data[[1]])){
        stop("The number of elements in variable_names must be the number of variables in data")
      }
    }

    if(is.null(times) == FALSE){
      if(is.integer(times) == FALSE & is.numeric(times) == FALSE){
        stop("The times must be numeric")
      }
      if(length(times) != ncol(data[[1]])){
        stop("The number of elements in times must be the number of times in data")
      }
    }

    cat("Computation of the scan statistic \n")
    signs <- multi_signs_matrix(data)
    index <- multi_fWMW(signs, matrix_clusters)
    cat("---Done--- \n")
    cat("Estimation of the statistical significance \n")
    nb_clusters <- ncol(matrix_clusters)
    nb_sites <- nrow(matrix_clusters)

    num_cores <- detectCores()

    if(nbCPU <=1 | num_cores <2){
      results <- pblapply(1:MC, function(i) multi_fWMW(lapply(1:nb_sites, function(s) signs[[permutations[i,s]]]), matrix_clusters))
    }else{

      if(num_cores < nbCPU){
        nbCPU <- num_cores
      }

      if(.Platform$OS.type == "windows"){
        cl <- makeCluster(nbCPU)
        results <- pblapply(1:MC, function(i) multi_fWMW(lapply(1:nb_sites, function(s) signs[[permutations[i,s]]]), matrix_clusters),cl=cl)
        stopCluster(cl)

      }else{
        results <- pblapply(1:MC, function(i) multi_fWMW(lapply(1:nb_sites, function(s) signs[[permutations[i,s]]]), matrix_clusters), cl = nbCPU)
      }

    }
    results <- matrix(unlist(results), ncol = nb_clusters, byrow = TRUE)
    stat_MC <- rowMaxs(results)
    pvals <- sapply(1:nb_clusters, function(j) (length(which(stat_MC >= index[j]))+1)/(MC+1))
    cat("---Done--- \n")
    cat("Finalization \n")
    index_clusters_temp <- which(pvals <= typeI)

    finalization <- FinScan(index_clusters_temp, index, filtering_post, type_minimaxi_post, mini_post, maxi_post, nb_sites, matrix_clusters, radius, areas, centres, pvals)


    return(ResScanOutputMultiFunct(sites_clusters = finalization$sites_clusters, pval_clusters = finalization$pval_clusters, centres_clusters = finalization$centres_clusters, radius_clusters = finalization$radius_clusters, areas_clusters = finalization$areas_clusters, system = system, times = times, variable_names = variable_names, sites_coord = sites_coord, data = data, method = "NPFSS"))

  }else{
    if(is(data, "matrix")){
      # univariate
      if(nrow(matrix_clusters)!=nrow(data)){
        stop("The number of sites must be the same in the matrix of clusters and in the data")
      }
      if(ncol(data)==1){
        stop("There must be at least two observation times")
      }

      if(is.null(times) == FALSE){
        if(is.integer(times) == FALSE & is.numeric(times) == FALSE){
          stop("The times must be numeric")
        }
        if(length(times) != ncol(data)){
          stop("The number of elements in times must be the number of times in data")
        }
      }
      cat("Computation of the scan statistic \n")
      signs <- uni_signs_matrix(data)
      index <- uni_fWMW(signs, matrix_clusters)
      cat("---Done--- \n")
      cat("Estimation of the statistical significance \n")
      nb_clusters <- ncol(matrix_clusters)
      nb_sites <- nrow(matrix_clusters)

      num_cores <- detectCores()

      if(nbCPU <=1 | num_cores <2){
        results <- pblapply(1:MC, function(i) uni_fWMW(signs[permutations[i,],], matrix_clusters))
      }else{

        if(num_cores < nbCPU){
          nbCPU <- num_cores
        }

        if(.Platform$OS.type == "windows"){
          cl <- makeCluster(nbCPU)
          results <- pblapply(1:MC, function(i) uni_fWMW(signs[permutations[i,],], matrix_clusters),cl=cl)
          stopCluster(cl)

        }else{
          results <- pblapply(1:MC, function(i) uni_fWMW(signs[permutations[i,],], matrix_clusters), cl = nbCPU)
        }

      }
      results <- matrix(unlist(results), ncol = nb_clusters, byrow = TRUE)
      stat_MC <- rowMaxs(results)
      pvals <- sapply(1:nb_clusters, function(j) (length(which(stat_MC >= index[j]))+1)/(MC+1))
      cat("---Done--- \n")
      cat("Finalization \n")
      index_clusters_temp <- which(pvals <= typeI)

      finalization <- FinScan(index_clusters_temp, index, filtering_post, type_minimaxi_post, mini_post, maxi_post, nb_sites, matrix_clusters, radius, areas, centres, pvals)


      return(ResScanOutputUniFunct(sites_clusters = finalization$sites_clusters, pval_clusters = finalization$pval_clusters, centres_clusters = finalization$centres_clusters, radius_clusters = finalization$radius_clusters, areas_clusters = finalization$areas_clusters, system = system, times = times, sites_coord = sites_coord, data = data, method = "NPFSS"))

    }else{
      stop("The data must be a list (multivariate case) or a matrix (univariate case)")
    }
  }

}


################################################################
##' @title PFSS scan procedure
##'
##' @description This function computes the PFSS (Parametric Functional scan statistic).
##'
##' @param data matrix. Matrix of the data, the rows correspond to the sites (or to the individuals if the observations are by individuals and not by sites) and each column represents an observation time. The times must be equally spaced and the same for each site/individual.
##' @param MC numeric. Number of Monte-Carlo permutations to evaluate the statistical significance of the clusters. By default: 999.
##' @param typeI numeric. The desired type I error. A cluster will be evaluated as significant if its associated p-value is less than typeI. By default 0.05.
##' @param nbCPU numeric. Number of CPU. If nbCPU > 1 parallelization is done. By default: 1.
##' @param times numeric. Times of observation of the data. By default NULL.
##' @param initialization list. Initialization for the scan procedure (see \code{\link{InitScan}} for more details).
##' @param permutations matrix. Indices of permutations of the data.
##'
##'
##' @return An object of class ResScanOutputUniFunct.
##'
##' @references Camille Frévent and Mohamed-Salem Ahmed and Matthieu Marbac and Michaël Genin (2021). Detecting Spatial Clusters in Functional Data: New Scan Statistic Approaches. Spatial Statistics, 46.
##'
##'
PFSS <- function(data, MC = 999, typeI = 0.05, nbCPU = 1, times = NULL, initialization, permutations){

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
      stop("There must be at least two observation times")
    }

    if(is.null(times) == FALSE){
      if(is.integer(times) == FALSE & is.numeric(times) == FALSE){
        stop("The times must be numeric")
      }
      if(length(times) != ncol(data)){
        stop("The number of elements in times must be the number of times in data")
      }
    }
    cat("Computation of the scan statistic \n")
    index <- fanova_cpp(data, matrix_clusters)
    cat("---Done--- \n")
    cat("Estimation of the statistical significance \n")
    nb_clusters <- ncol(matrix_clusters)
    nb_sites <- nrow(matrix_clusters)

    num_cores <- detectCores()

    if(nbCPU <=1 | num_cores <2){
      results <- pblapply(1:MC, function(i) fanova_cpp(data[permutations[i,],], matrix_clusters))
    }else{
      if(num_cores < nbCPU){
        nbCPU <- num_cores
      }

      if(.Platform$OS.type == "windows"){
        cl <- makeCluster(nbCPU)
        results <- pblapply(1:MC, function(i) fanova_cpp(data[permutations[i,],], matrix_clusters),cl=cl)
        stopCluster(cl)

      }else{
        results <- pblapply(1:MC, function(i) fanova_cpp(data[permutations[i,],], matrix_clusters), cl = nbCPU)
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

  return(ResScanOutputUniFunct(sites_clusters = finalization$sites_clusters, pval_clusters = finalization$pval_clusters, centres_clusters = finalization$centres_clusters, radius_clusters = finalization$radius_clusters, areas_clusters = finalization$areas_clusters, system = system, times = times, sites_coord = sites_coord, data = data, method = "PFSS"))

}


################################################################
##' @title MPFSS scan procedure
##'
##' @description This function computes the MPFSS (Parametric Multivariate Functional scan statistic).
##'
##' @param data list of numeric matrices. List of nb_sites (or nb_individuals if the observations are by individuals and not by sites) matrices of the data, the rows correspond to the variables and each column represents an observation time. The times must be equally spaced and the same for each site/individual.
##' @param MC numeric. Number of Monte-Carlo permutations to evaluate the statistical significance of the clusters. By default: 999.
##' @param typeI numeric. The desired type I error. A cluster will be evaluated as significant if its associated p-value is less than typeI. By default 0.05.
##' @param method character vector. The methods to compute the significant clusters. Options: "LH", "W", "P", "R" for respectively the Lawley-Hotelling trace test statistic, The Wilks lambda test statistic, the Pillai trace test statistic and the Roy's maximum root test statistic. By default all are computed.
##' @param nbCPU numeric. Number of CPU. If nbCPU > 1 parallelization is done. By default: 1.
##' @param variable_names character. Names of the variables. By default NULL.
##' @param times numeric. Times of observation of the data. By default NULL.
##' @param initialization list. Initialization for the scan procedure (see \code{\link{InitScan}} for more details).
##' @param permutations matrix. Indices of permutations of the data.
##'
##'
##' @return List of objects of class ResScanOutputMultiFunct (one element by method)
##'
##' @references Camille Frévent and Mohamed-Salem Ahmed and Sophie Dabo-Niang and Michaël Genin (2023). Investigating Spatial Scan Statistics for Multivariate Functional Data. Journal of the Royal Statistical Society Series C: Applied Statistics, 72(2), 450-475.
##'
##'
MPFSS <- function(data, MC = 999, typeI = 0.05, method = c("LH","W","P","R"), nbCPU = 1, variable_names = NULL, times = NULL, initialization, permutations){

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

  if(is(data, "list")){
    if(length(data)!=nrow(matrix_clusters)){
      stop("The data must contain the same number of sites than the matrix of clusters")
    }
    nb_rows <- sapply(1:length(data), function(r) nrow(data[[r]]))
    nb_cols <- sapply(1:length(data), function(r) ncol(data[[r]]))

    if(sum((nb_rows!=nb_rows[1])) + sum((nb_cols!=nb_cols[1])) > 0){
      stop("All matrices of the list must be of the same dimensions")
    }
    if(nb_rows[1]==1){
      stop("There must be at least two variables (rows)")
    }
    if(nb_cols[1]==1){
      stop("There must be at least two observation times (columns)")
    }
    if(sum(!(method %in% c("LH","W","P","R")))>0){
      stop("The methods must be among LH, W, P and R")
    }

    if(is.null(variable_names) == FALSE){
      if(is(variable_names, "character") == FALSE){
        stop("variable_names must be character")
      }
      if(length(variable_names) != nrow(data[[1]])){
        stop("The number of elements in variable_names must be the number of variables in data")
      }
    }

    if(is.null(times) == FALSE){
      if(is.integer(times) == FALSE & is.numeric(times) == FALSE){
        stop("The times must be numeric")
      }
      if(length(times) != ncol(data[[1]])){
        stop("The number of elements in times must be the number of times in data")
      }
    }

    nb_clusters <- ncol(matrix_clusters)
    nb_sites <- nrow(matrix_clusters)
    nb_times <- ncol(data[[1]])
    nb_var <- nrow(data[[1]])
    cat("Computation of the scan statistic \n")
    mean_tot <- matrix(0, nrow = nb_var, ncol = nb_times)
    for(i in 1:nb_sites){
      mean_tot <- mean_tot + data[[i]]
    }
    mean_tot <- mean_tot / nb_sites

    cst1 <- matrix(0, nrow = nb_var, ncol = nb_var)
    for(i in 1:nb_sites){
      cst1 <- cst1 + data[[i]] %*% t(data[[i]])
    }
    cst1 <- cst1 / nb_times
    cst2 <- mean_tot %*% t(mean_tot)
    cst2 <- cst2 * nb_sites / nb_times

    index <- fmanova_cpp(data, matrix_clusters, cst1, cst2)
    index_LH <- as.vector(index$LH)
    index_W <- as.vector(index$W)
    index_P <- as.vector(index$P)
    index_R <- as.vector(index$R)
    cat("---Done--- \n")
    cat("Estimation of the statistical significance \n")
    num_cores <- detectCores()

    if(nbCPU <=1 | num_cores <2){
      results <- pblapply(1:MC, function(i) fmanova_cpp(lapply(1:nb_sites, function(s) data[[permutations[i,s]]]), matrix_clusters, cst1, cst2))
      results <- purrr::transpose(results)
    }else{
      if(num_cores < nbCPU){
        nbCPU <- num_cores
      }

      if(.Platform$OS.type == "windows"){
        cl <- makeCluster(nbCPU)
        results <- pblapply(1:MC, function(i) fmanova_cpp(lapply(1:nb_sites, function(s) data[[permutations[i,s]]]), matrix_clusters, cst1, cst2),cl=cl)
        stopCluster(cl)

      }else{
        results <- pblapply(1:MC, function(i) fmanova_cpp(lapply(1:nb_sites, function(s) data[[permutations[i,s]]]), matrix_clusters, cst1, cst2), cl = nbCPU)
      }

      results <- purrr::transpose(results)
    }
    cat("---Done--- \n")
    cat("Finalization \n")
    if("LH" %in% method){
      results_LH <- results$LH
      results_LH <- matrix(unlist(results_LH), ncol = nb_clusters, byrow = TRUE)
      stat_MC_LH <- rowMaxs(results_LH)
      pvals_LH <- sapply(1:nb_clusters, function(j) (length(which(stat_MC_LH >= index_LH[j]))+1)/(MC+1))
      index_clusters_LH_temp <- which(pvals_LH <= typeI)

      finalization_LH <- FinScan(index_clusters_LH_temp, index_LH, filtering_post, type_minimaxi_post, mini_post, maxi_post, nb_sites, matrix_clusters, radius, areas, centres, pvals_LH)

    }
    if("W" %in% method){
      results_W <- results$W
      results_W <- matrix(unlist(results_W), ncol = nb_clusters, byrow = TRUE)
      stat_MC_W <- rowMins(results_W)
      pvals_W <- sapply(1:nb_clusters, function(j) (length(which(stat_MC_W <= index_W[j]))+1)/(MC+1))
      index_clusters_W_temp <- which(pvals_W <= typeI)

      finalization_W <- FinScan(index_clusters_W_temp, index_W, filtering_post, type_minimaxi_post, mini_post, maxi_post, nb_sites, matrix_clusters, radius, areas, centres, pvals_W, maximize = FALSE)

    }
    if("P" %in% method){
      results_P <- results$P
      results_P <- matrix(unlist(results_P), ncol = nb_clusters, byrow = TRUE)
      stat_MC_P <- rowMaxs(results_P)
      pvals_P <- sapply(1:nb_clusters, function(j) (length(which(stat_MC_P >= index_P[j]))+1)/(MC+1))
      index_clusters_P_temp <- which(pvals_P <= typeI)

      finalization_P <- FinScan(index_clusters_P_temp, index_P, filtering_post, type_minimaxi_post, mini_post, maxi_post, nb_sites, matrix_clusters, radius, areas, centres, pvals_P)

    }
    if("R" %in% method){
      results_R <- results$R
      results_R <- matrix(unlist(results_R), ncol = nb_clusters, byrow = TRUE)
      stat_MC_R <- rowMaxs(results_R)
      pvals_R <- sapply(1:nb_clusters, function(j) (length(which(stat_MC_R >= index_R[j]))+1)/(MC+1))
      index_clusters_R_temp <- which(pvals_R <= typeI)

      finalization_R <- FinScan(index_clusters_R_temp, index_R, filtering_post, type_minimaxi_post, mini_post, maxi_post, nb_sites, matrix_clusters, radius, areas, centres, pvals_R)

    }
  }else{
      stop("The data must be a list")
  }

  output <- list()

  if("LH" %in% method){
    output[["LH"]] <- ResScanOutputMultiFunct(sites_clusters = finalization_LH$sites_clusters, pval_clusters = finalization_LH$pval_clusters, centres_clusters = finalization_LH$centres_clusters, radius_clusters = finalization_LH$radius_clusters, areas_clusters = finalization_LH$areas_clusters, system = system, times = times, variable_names = variable_names, sites_coord = sites_coord, data = data, method = "MPFSS Lawley-Hotelling")
  }
  if("W" %in% method){
    output[["W"]] <- ResScanOutputMultiFunct(sites_clusters = finalization_W$sites_clusters, pval_clusters = finalization_W$pval_clusters, centres_clusters = finalization_W$centres_clusters, radius_clusters = finalization_W$radius_clusters, areas_clusters = finalization_W$areas_clusters, system = system, times = times, variable_names = variable_names, sites_coord = sites_coord, data = data, method = "MPFSS Wilks")
  }
  if("P" %in% method){
    output[["P"]] <- ResScanOutputMultiFunct(sites_clusters = finalization_P$sites_clusters, pval_clusters = finalization_P$pval_clusters, centres_clusters = finalization_P$centres_clusters, radius_clusters = finalization_P$radius_clusters, areas_clusters = finalization_P$areas_clusters, system = system, times = times, variable_names = variable_names, sites_coord = sites_coord, data = data, method = "MPFSS Pillai")
  }
  if("R" %in% method){
    output[["R"]] <- ResScanOutputMultiFunct(sites_clusters = finalization_R$sites_clusters, pval_clusters = finalization_R$pval_clusters, centres_clusters = finalization_R$centres_clusters, radius_clusters = finalization_R$radius_clusters, areas_clusters = finalization_R$areas_clusters, system = system, times = times, variable_names = variable_names, sites_coord = sites_coord, data = data, method = "MPFSS Roy")
  }

  return(output)
}



################################################################
##' @title DFFSS scan procedure
##'
##' @description This function computes the DFFSS (Distribution-Free Functional scan statistic).
##'
##' @param data matrix. Matrix of the data, the rows correspond to the sites (or to the individuals if the observations are by individuals and not by sites) and each column represents an observation time. The times must be the same for each site/individual.
##' @param MC numeric. Number of Monte-Carlo permutations to evaluate the statistical significance of the clusters. By default: 999.
##' @param typeI numeric. The desired type I error. A cluster will be evaluated as significant if its associated p-value is less than typeI. By default 0.05.
##' @param nbCPU numeric. Number of CPU. If nbCPU > 1 parallelization is done. By default: 1.
##' @param times numeric. Times of observation of the data. By default NULL.
##' @param initialization list. Initialization for the scan procedure (see \code{\link{InitScan}} for more details).
##' @param permutations matrix. Indices of permutations of the data.
##'
##'
##' @return An object of class ResScanOutputUniFunct.
##'
##' @references Camille Frévent and Mohamed-Salem Ahmed and Matthieu Marbac and Michaël Genin (2021). Detecting Spatial Clusters in Functional Data: New Scan Statistic Approaches. Spatial Statistics, 46.
##'
##'
DFFSS <- function(data, MC = 999, typeI = 0.05, nbCPU = 1, times = NULL, initialization, permutations){

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
      stop("There must be at least two observation times")
    }
    if(is.null(times) == FALSE){
      if(is.integer(times) == FALSE & is.numeric(times) == FALSE){
        stop("The times must be numeric")
      }
      if(length(times) != ncol(data)){
        stop("The number of elements in times must be the number of times in data")
      }
    }

    nb_sites <- nrow(matrix_clusters)
    cat("Computation of the scan statistic \n")
    index <- pointwise_dfree(data, matrix_clusters)
    cat("---Done--- \n")
    cat("Estimation of the statistical significance \n")
    nb_clusters <- ncol(matrix_clusters)
    nb_sites <- nrow(matrix_clusters)

    num_cores <- detectCores()

    if(nbCPU <=1 | num_cores <2){
      results <- pblapply(1:MC, function(i) pointwise_dfree(data[permutations[i,],], matrix_clusters))
    }else{
      if(num_cores < nbCPU){
        nbCPU <- num_cores
      }

      if(.Platform$OS.type == "windows"){
        cl <- makeCluster(nbCPU)
        results <- pblapply(1:MC, function(i) pointwise_dfree(data[permutations[i,],], matrix_clusters),cl=cl)
        stopCluster(cl)

      }else{
        results <- pblapply(1:MC, function(i) pointwise_dfree(data[permutations[i,],], matrix_clusters), cl = nbCPU)
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

  return(ResScanOutputUniFunct(sites_clusters = finalization$sites_clusters, pval_clusters = finalization$pval_clusters, centres_clusters = finalization$centres_clusters, radius_clusters = finalization$radius_clusters, areas_clusters = finalization$areas_clusters, system = system, times = times, sites_coord = sites_coord, data = data, method = "DFSS"))

}


################################################################
##' @title MDFFSS scan procedure
##'
##' @description This function computes the MDFFSS (Multivariate Distribution-Free Functional scan statistic).
##'
##' @param data list of numeric matrices. List of nb_sites (or nb_individuals if the observations are by individuals and not by sites) matrices of the data, the rows correspond to the variables and each column represents an observation time. The times must be the same for each site/individual.
##' @param MC numeric. Number of Monte-Carlo permutations to evaluate the statistical significance of the clusters. By default: 999.
##' @param typeI numeric. The desired type I error. A cluster will be evaluated as significant if its associated p-value is less than typeI. By default 0.05.
##' @param nbCPU numeric. Number of CPU. If nbCPU > 1 parallelization is done. By default: 1.
##' @param variable_names character. Names of the variables. By default NULL.
##' @param times numeric. Times of observation of the data. By default NULL.
##' @param initialization list. Initialization for the scan procedure (see \code{\link{InitScan}} for more details).
##' @param permutations matrix. Indices of permutations of the data.
##'
##' @return An object of class ResScanOutputMultiFunct.
##'
##' @references  Camille Frévent and Mohamed-Salem Ahmed and Sophie Dabo-Niang and Michaël Genin (2023). Investigating Spatial Scan Statistics for Multivariate Functional Data. Journal of the Royal Statistical Society Series C: Applied Statistics, 72(2), 450-475.
##'
##'
MDFFSS <- function(data, MC = 999, typeI = 0.05, nbCPU = 1, variable_names = NULL, times = NULL, initialization, permutations){
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
  if(is(data, "list")){
    if(length(data)!=nrow(matrix_clusters)){
      stop("The data must contain the same number of sites than the matrix of clusters")
    }
    nb_rows <- sapply(1:length(data), function(r) nrow(data[[r]]))
    nb_cols <- sapply(1:length(data), function(r) ncol(data[[r]]))

    if(sum((nb_rows!=nb_rows[1])) + sum((nb_cols!=nb_cols[1])) > 0){
      stop("All matrices of the list must be of the same dimensions")
    }
    if(nb_rows[1]==1){
      stop("There must be at least two variables (rows)")
    }
    if(nb_cols[1]==1){
      stop("There must be at least two observation times (columns)")
    }

    if(is.null(variable_names) == FALSE){
      if(is(variable_names, "character") == FALSE){
        stop("variable_names must be character")
      }
      if(length(variable_names) != nrow(data[[1]])){
        stop("The number of elements in variable_names must be the number of variables in data")
      }
    }

    if(is.null(times) == FALSE){
      if(is.integer(times) == FALSE & is.numeric(times) == FALSE){
        stop("The times must be numeric")
      }
      if(length(times) != ncol(data[[1]])){
        stop("The number of elements in times must be the number of times in data")
      }
    }
    cat("Computation of the scan statistic \n")
    nb_sites <- nrow(matrix_clusters)
    nb_clusters <- ncol(matrix_clusters)
    index <- dfree_index_multi(data, matrix_clusters)
    cat("---Done--- \n")
    cat("Estimation of the statistical significance \n")
    num_cores <- detectCores()

    if(nbCPU <=1 | num_cores <2){
      results <- pblapply(1:MC, function(i) dfree_index_multi(lapply(1:nb_sites, function(s) data[[permutations[i,s]]]), matrix_clusters))
    }else{
      if(num_cores < nbCPU){
        nbCPU <- num_cores
      }

      if(.Platform$OS.type == "windows"){
        cl <- makeCluster(nbCPU)
        results <- pblapply(1:MC, function(i) dfree_index_multi(lapply(1:nb_sites, function(s) data[[permutations[i,s]]]), matrix_clusters),cl=cl)
        stopCluster(cl)

      }else{
        results <- pblapply(1:MC, function(i) dfree_index_multi(lapply(1:nb_sites, function(s) data[[permutations[i,s]]]), matrix_clusters), cl = nbCPU)
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
    stop("The data must be a list")
  }


  return(ResScanOutputMultiFunct(sites_clusters = finalization$sites_clusters, pval_clusters = finalization$pval_clusters, centres_clusters = finalization$centres_clusters, radius_clusters = finalization$radius_clusters, areas_clusters = finalization$areas_clusters, system = system, times = times, variable_names = variable_names, sites_coord = sites_coord, data = data, method = "MDFSS"))

}


################################################################
##' @title MRBFSS scan procedure
##'
##' @description This function computes the MRBFSS (Multivariate Rank-Based Functional scan statistic).
##'
##' @param data list of numeric matrices. List of nb_sites (or nb_individuals if the observations are by individuals and not by sites) matrices of the data, the rows correspond to the variables and each column represents an observation time. The times must be the same for each site/individual.
##' @param MC numeric. Number of Monte-Carlo permutations to evaluate the statistical significance of the clusters. By default: 999.
##' @param typeI numeric. The desired type I error. A cluster will be evaluated as significant if its associated p-value is less than typeI. By default 0.05.
##' @param nbCPU numeric. Number of CPU. If nbCPU > 1 parallelization is done. By default: 1.
##' @param variable_names character. Names of the variables. By default NULL.
##' @param times numeric. Times of observation of the data. By default NULL.
##' @param initialization list. Initialization for the scan procedure (see \code{\link{InitScan}} for more details).
##' @param permutations matrix. Indices of permutations of the data.
##'
##'
##' @return An object of class ResScanOutputMultiFunct
##'
##' @references Camille Frévent and Mohamed-Salem Ahmed and Sophie Dabo-Niang and Michaël Genin (2023). Investigating Spatial Scan Statistics for Multivariate Functional Data. Journal of the Royal Statistical Society Series C: Applied Statistics, 72(2), 450-475.
##'
##'
MRBFSS <- function(data, MC = 999, typeI = 0.05, nbCPU = 1, variable_names = NULL, times = NULL, initialization, permutations){

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

  if(is(data, "list")){
    if(length(data)!=nrow(matrix_clusters)){
      stop("The data must contain the same number of sites than the matrix of clusters")
    }
    nb_rows <- sapply(1:length(data), function(r) nrow(data[[r]]))
    nb_cols <- sapply(1:length(data), function(r) ncol(data[[r]]))

    if(sum((nb_rows!=nb_rows[1])) + sum((nb_cols!=nb_cols[1])) > 0){
      stop("All matrices of the list must be of the same dimensions")
    }
    if(nb_rows[1]==1){
      stop("There must be at least two variables (rows)")
    }
    if(nb_cols[1]==1){
      stop("There must be at least two observation times (columns)")
    }

    if(is.null(variable_names) == FALSE){
      if(is(variable_names, "character") == FALSE){
        stop("variable_names must be character")
      }
      if(length(variable_names) != nrow(data[[1]])){
        stop("The number of elements in variable_names must be the number of variables in data")
      }
    }

    if(is.null(times) == FALSE){
      if(is.integer(times) == FALSE & is.numeric(times) == FALSE){
        stop("The times must be numeric")
      }
      if(length(times) != ncol(data[[1]])){
        stop("The number of elements in times must be the number of times in data")
      }
    }

    cat("Computation of the scan statistic \n")
    new_data <- transform_data(data)

    nb_clusters <- ncol(matrix_clusters)
    nb_sites <- nrow(matrix_clusters)
    nb_times <- ncol(data[[1]])

    index <- pointwise_wmw_multi(new_data, matrix_clusters)
    cat("---Done--- \n")
    cat("Estimation of the statistical significance \n")
    num_cores <- detectCores()

    if(nbCPU <=1 | num_cores <2){
      results <- pblapply(1:MC, function(i) pointwise_wmw_multi(lapply(1:nb_times, function(t) new_data[[t]][permutations[i,],]), matrix_clusters))
    }else{

      if(num_cores < nbCPU){
        nbCPU <- num_cores
      }

      if(.Platform$OS.type == "windows"){
        cl <- makeCluster(nbCPU)
        results <- pblapply(1:MC, function(i) pointwise_wmw_multi(lapply(1:nb_times, function(t) new_data[[t]][permutations[i,],]), matrix_clusters),cl=cl)
        stopCluster(cl)

      }else{
        results <- pblapply(1:MC, function(i) pointwise_wmw_multi(lapply(1:nb_times, function(t) new_data[[t]][permutations[i,],]), matrix_clusters), cl = nbCPU)
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
    stop("The data must be a list")
  }


  return(ResScanOutputMultiFunct(sites_clusters = finalization$sites_clusters, pval_clusters = finalization$pval_clusters, centres_clusters = finalization$centres_clusters, radius_clusters = finalization$radius_clusters, areas_clusters = finalization$areas_clusters, system = system, times = times, variable_names = variable_names, sites_coord = sites_coord, data = data, method = "MRBFSS"))

}



################################################################
##' @title URBFSS scan procedure
##'
##' @description This function computes the URBFSS (Univariate Rank-Based Functional scan statistic).
##'
##' @param data matrix. Matrix of the data, the rows correspond to the sites (or to the individuals if the observations are by individuals and not by sites) and each column represents an observation time. The times must be the same for each site/individual.
##' @param MC numeric. Number of Monte-Carlo permutations to evaluate the statistical significance of the clusters. By default: 999.
##' @param typeI numeric. The desired type I error. A cluster will be evaluated as significant if its associated p-value is less than typeI. By default 0.05.
##' @param nbCPU numeric. Number of CPU. If nbCPU > 1 parallelization is done. By default: 1.
##' @param times numeric. Times of observation of the data. By default NULL.
##' @param initialization list. Initialization for the scan procedure (see \code{\link{InitScan}} for more details).
##' @param permutations matrix. Indices of permutations of the data.
##'
##'
##' @return An object of class ResScanOutputUniFunct.
##'
##' @seealso \code{\link{MRBFSS}} which is the multivariate version of the URBFSS
##'
##'
URBFSS <- function(data, MC = 999, typeI = 0.05, nbCPU = 1, times = NULL, initialization, permutations){

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
      stop("There must be at least two observation times")
    }
    if(is.null(times) == FALSE){
      if(is.integer(times) == FALSE & is.numeric(times) == FALSE){
        stop("The times must be numeric")
      }
      if(length(times) != ncol(data)){
        stop("The number of elements in times must be the number of times in data")
      }
    }
    cat("Computation of the scan statistic \n")
    nb_sites <- nrow(matrix_clusters)

    rank_data <- t(colRanks(data, ties.method = "average"))

    index <- pointwise_wmw_uni(rank_data, matrix_clusters)
    cat("---Done--- \n")
    cat("Estimation of the statistical significance \n")
    nb_clusters <- ncol(matrix_clusters)
    nb_sites <- nrow(matrix_clusters)

    num_cores <- detectCores()

    if(nbCPU <=1 | num_cores <2){
      results <- pblapply(1:MC, function(i) pointwise_wmw_uni(rank_data[permutations[i,],], matrix_clusters))
    }else{
      if(num_cores < nbCPU){
        nbCPU <- num_cores
      }

      if(.Platform$OS.type == "windows"){
        cl <- makeCluster(nbCPU)
        results <- pblapply(1:MC, function(i) pointwise_wmw_uni(rank_data[permutations[i,],], matrix_clusters),cl=cl)
        stopCluster(cl)

      }else{
        results <- pblapply(1:MC, function(i) pointwise_wmw_uni(rank_data[permutations[i,],], matrix_clusters), cl = nbCPU)
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

  return(ResScanOutputUniFunct(sites_clusters = finalization$sites_clusters, pval_clusters = finalization$pval_clusters, centres_clusters = finalization$centres_clusters, radius_clusters = finalization$radius_clusters, areas_clusters = finalization$areas_clusters, system = system, times = times, sites_coord = sites_coord, data = data, method = "URBFSS"))
}
