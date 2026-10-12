#' SparseDC Multi
#'
#' Applies sparse clustering to data from multiple conditions, linking the
#' clusters across conditions and selecting a set of marker variables for
#' each cluster and condition. See the manuscript for descriptions of the
#' different categories of marker genes.
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
#'
#' @return A list containing cluster assignments, center values and the scores
#' for each start.
#' @export
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
#' lambda1 <- lambda1_calculator(pdat, dim = 3, nclust = 3)
#' lambda2 <- lambda2_calculator(pdat, dim = 3, nclust = 3, lambda1 = lambda1)
#' smdc_res <- sparse_mdc(pdat, nclust = 3, dim = 3, lambda1 = lambda1,
#' lambda2 = lambda2)
sparse_mdc <- function(pdat, nclust, dim, lambda1, lambda2, nitter = 20,
                      nstarts = 50, init_iter = 5, delta = 0.0000001){
  cluster_list <- center_list <- vector(dim, mode="list")
  iter_star <- NA
  c_dat <- pdat[[1]]
  ngenes <- nrow(c_dat)
  for(d in 2:dim){
    c_dat <- cbind(c_dat, pdat[[d]])
  }

  center_starts <- vector(nstarts, mode = "list")
  cat("Calculating start values", fill=TRUE)
  for(s in 1:nstarts){
    center_starts[[s]] <- t(stats::kmeans(x =t(c_dat), centers=nclust,
                                          nstart=1,
                                          iter.max = init_iter)$centers)
  }
  rm(c_dat)
  center_start <- center_starts[!duplicated(center_starts)]
  nstarts_r <- length(center_start)
  cat("The number of unique start values is: ", nstarts_r, fill=TRUE)
  score_vec <- rep(NA, nstarts_r)  # Create vector to contain scores for each of the starts
  for(s in 1:nstarts_r) { # For each start
    cat("Start number: ", s, fill=TRUE)
    clust_hist <- vector(nitter, mode="list")
    clusters_list_t <- mu <- vector(dim, mode="list")
    for(d in 1:dim){
      mu[[d]] <- center_start[[s]] # Set all centers equal to starting centers
    }
    iter_num <- 0  # Set iteration number to zero
    clus_same <- FALSE  # Set cluster same indicator to FALSE
    while(iter_num < nitter & clus_same == FALSE){
      iter_num <- iter_num + 1
      cat("Iteration: ", iter_num, fill=TRUE)
      clusters_list_t <- update_c(mu = mu, pdat = pdat, nclust = nclust ,
                                  dim = dim, lambda1 = lambda1,
                                  lambda2 = lambda2, delta = delta)
      mu <- update_mu(clusters = clusters_list_t, pdat = pdat, nclust = nclust,
                      dim = dim, lambda1 = lambda1, lambda2 = lambda2,
                      ngenes = ngenes, delta = delta)
      clust_hist[[iter_num]] <- clusters_list_t
      if (iter_num > 1){
        if (identical(clust_hist[[(iter_num - 1)]], clust_hist[[iter_num]])){
          clus_same <-  TRUE
          cat("Clusters are unchanged!", fill=TRUE)
        }
      }
    }
    score_store <- score_calc(pdat = pdat, clusters = clusters_list_t, mu = mu,
                              lambda1 = lambda1, lambda2 = lambda2,
                              nclust = nclust, delta = delta, dim = dim)
    score_vec[s] <- score_store[[1]]
    cat("The score for start ", s, " is ", score_vec[s], fill=TRUE)
    if (score_vec[s] <= min(stats::na.omit(score_vec))){
      cluster_list <- clusters_list_t
      center_list <- mu
      cat("New minimum score!", fill=TRUE)
      score_breakdown <- score_store[[2]]
    }
  }
  return(list(clusters = cluster_list, centers = center_list,
              scores = score_vec, score_store = score_store))
}



