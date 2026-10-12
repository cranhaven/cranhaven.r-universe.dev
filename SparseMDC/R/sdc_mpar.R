#' SparseDC Multi Parallel
#'
#' Applies sparse clustering to data from multiple conditions, linking the
#' clusters across conditions and selecting a set of marker variables for
#' each cluster and condition. This is a wrapper function to run SparseMDC in
#' parallel and choose the solution with the mimimum score for each run.
#'
#' @param pdat list with D entries, each entry contains data d, p * n matrix.
#' This data should be centered and log-transformed.
#' @param nclust Number of clusters in the data.
#' @param dim Total number of conditions, D.
#' @param lambda1 The lambda 1 value to use in the SparseMDC function. This
#' value controls the number of marker genes detected for each of the clusters
#' in the final result. This can be calculated using the "lambda1_calculator"
#' function or supplied by the user.
#' @param lambda2 The lambda 2 value to use in the SparseMDC function. This
#' value controls the number of genes that show condition-dependent
#' expression within each cell type. This can be calculated using the
#' "lambda2_calculator" function or supplied by the user.
#' @param nitter The max number of iterations for each of the start values, the
#' default value is 20.
#' @param nstarts The max number of possible starts. The default
#' value is 50.
#' @param init_iter The number of iterations used to initialize the
#' algorithm. Higher values result in less starts but more accurate and
#' vice versa. Default is 5.
#' @param delta Small term to ensure existance of solution, default is
#'  0.0000001.
#' @param par_starts Number of parallel starts.
#' @param cores Number of cores to use.
#'
#' @return A list containing cluster assignments, center values and the scores
#' for each start.
#'
#' @importFrom doRNG %dorng%
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#'
#' @export
#'
#' @examples
#' set.seed(10)
#' # Select small dataset for example
#' data_test <- data_biase[1:100,]
#' # Split data into condition A and B
#' data_A <- data_test[ , which(condition_biase == "A")]
#' data_B <- data_test[ , which(condition_biase == "B")]
#' data_C <- data_test[ , which(condition_biase == "C")]
#' # Store data as list
#' dat_l <- list(data_A, data_B, data_C)
#' # Pre-process the data
#' pdat <- pre_proc_data(dat_l, dim=3, norm = FALSE, log = TRUE,
#' center = TRUE)
#' # Calculate lambda1
#' lambda1 <- lambda1_calculator(pdat, dim = 3, nclust = 3)
#' # Calcualte lambda2
#' lambda2 <- lambda2_calculator(pdat, dim = 3, nclust = 3, lambda1 = lambda1)
#' # Prepare parallel enviornment
#' library(doParallel) # Load package
#' library(foreach)  # Load the package
#' library(doRNG)
#' # Apply SparseMDC
#' smdc_res <- sdc_mpar(pdat, nclust = 3, dim = 3, lambda1 = lambda1,
#' lambda2 = lambda2, par_starts = 2, cores = 2)
sdc_mpar <- function(pdat, nclust, dim, lambda1, lambda2, nitter = 20,
                     nstarts = 50, init_iter = 5, delta = 0.0000001,
                     par_starts, cores){
  clus_1 <- parallel::makeCluster(cores) # Create a cluster with cores
  doParallel::registerDoParallel(clus_1) # Register the cluster
  doRNG::registerDoRNG(1)

  res_all <- foreach::foreach(p = 1:par_starts, .packages="SparseMDC") %dorng% {
    fit_1 <- sparse_mdc(pdat = pdat, nclust = nclust, dim = dim,
                       lambda1 = lambda1, lambda2 = lambda2, nitter = nitter,
                       nstarts = nstarts, init_iter = init_iter, delta = delta)
    return(fit_1)
  }
  parallel::stopCluster(clus_1)
  best_par <- rep(NA, par_starts)
  for( p in 1:par_starts){
    best_par[p] <- min(res_all[[p]][[3]])
  }
  best_res <- res_all[[which.min(best_par)]]
  return(best_res)
}
