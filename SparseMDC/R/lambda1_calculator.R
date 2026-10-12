#' Lambda 1 Calcualtor
#'
#' Calculates the lambda 1 value for the SparseMDC algorithm. The lambda 1
#' value controls the number of marker genes selected for each
#' cluster in the output from SparseMDC. It is calculated as the value of
#' lambda 1 that results in no marker genes being selected when then are no
#' meaningful clusters present in the data. Please see the original manuscript
#' for further details.
#'
#' @param dat_l list with D entries, each entry contains data d, p * n matrix.
#' This data should be centered and log-transformed.
#' @param dim Total number of conditions, D.
#' @param nclust Total number of clusters.
#' @param nboot The number of bootstrap repetitions used for estimating
#' lambda 1, the default value is 1000.
#' @param delta Small value term added to ensure existance, default value is
#' 0.0000001.
#' @param alpha1 The quantile of the bootstrapped lambda 1 values to use,
#' range is (0,1). The default value is 0.5, the median of the calculated
#' lambda 1 values.
#'
#' @return The estimated value of lambda1 for use in main SparseMDC
#' algorithm
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
#' lambda1 <- lambda1_calculator(pdat, dim = 3, nclust = 3 )
#'
lambda1_calculator <- function(dat_l, dim, nclust, nboot = 1000, alpha1 = 0.5,
                               delta =0.0000001){
  temp_mat <- matrix(unlist(dat_l), nrow=nrow(dat_l[[1]]))
  l1_vec <- rep(0, nboot)
  n_d <- ncol(temp_mat)
  for( i in 1:nboot){

      t_clus <- sample(nclust, ncol(temp_mat), replace=TRUE)

    clus_vals <- unique(t_clus)
    l1_vec_boot <- matrix(0, ncol = max(t_clus), nrow=nrow(temp_mat))
    for (k in clus_vals){
      n_k <- sum(t_clus == k)
      m_vals <- (rowMeans(temp_mat[,which(t_clus == k), drop=FALSE]) *
                   ((n_k)/ (n_k + delta)))
      l1_vec_boot[,k] <- abs(m_vals)
    }
    l1_vec[i] <- max(l1_vec_boot, na.rm =TRUE)
  }
  l1 <- unname(stats::quantile(l1_vec, alpha1))
  return(rep(l1, dim))
}
