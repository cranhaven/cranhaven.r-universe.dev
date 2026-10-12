#' Lambda 2 Calculator
#'
#' Calculates the lambda 2 values for use in the main SparseMDC algorithm, the
#' lambda 2 value controls the number of genes that show condition-dependent
#' expression within each cell type. That is it controls the number of
#' different mean values across the conditions for each cluster. It is
#' calculated by estimating the value of lambda 2 that would result in no
#' difference in mean values across conditions when there are no meaningful
#' differences across between the conditions. For further details please see
#' the original manuscript.
#'
#' @param dat_l list with D entries, each entry contains data d, p * n matrix.
#' This data should be centered and log-transformed.
#' @param dim Total number of conditions, D.
#' @param nclust Total number of clusters.
#' @param nboot The number of bootstrap repetitions for estimating lambda 2,
#' the default value is 1000.
#' @param alpha2 The quantile of the bootstrapped lambda 2 values to use,
#' range is (0,1). The default value is 0.5, the median of the calculated
#' lambda 2 values.
#' @param delta Small term to ensure existance of solution, default is
#'  0.0000001.
#' @param lambda1 Calcualted penalty parameter for mean size.
#'
#' @return The estimated value of lambda2
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
#' lambda1 <- lambda1_calculator(pdat, dim = 3, nclust = 3)
#' lambda2 <- lambda2_calculator(pdat, dim = 3, nclust = 3, lambda1 = lambda1)
lambda2_calculator <- function(dat_l, dim, nclust, nboot = 1000, alpha2 = 0.5,
                               delta = 0.0000001, lambda1){
  l2_vec <- rep(NA, dim-1)
  for(j in 1:(dim - 1)){
    temp_mat <- matrix(unlist(dat_l[(j):(j+1)]), nrow=nrow(dat_l[[1]]))
    l2_vec_boot <- rep(NA, nboot)
    n_d <- ncol(temp_mat)
    for( i in 1:nboot){

      t_clus <- sample(nclust, ncol(temp_mat), replace=TRUE)
      t_dim <- sample(2, ncol(temp_mat), replace= TRUE)
      clus_vals <- unique(t_clus)
      l2_clus_boot <- rep(0, nclust)

      for( k in clus_vals){


        n_d <- sum(t_dim == 1)
        n_d1 <- sum(t_dim == 2)
        n_1 <- sum(t_clus == k  & t_dim == 1)
        n_2 <- sum(t_clus == k  & t_dim == 2)
        m_1 <- m_2 <- rep(NA, nrow(dat_l[[1]]))
        if ( n_1 > 0){
          m_1 <- rowMeans(temp_mat[,which(t_clus == k & t_dim == 1), drop=FALSE])
          s_1 <- lambda1[j] * ((n_1 + delta) / (n_1))
          u_1 <- S_func(m_1, s_1)

        } else if (n_1 == 0){
          u_1 <- 0
        }
        if ( n_2 > 0){
          m_2 <- rowMeans(temp_mat[,which(t_clus == k & t_dim == 2), drop=FALSE])
          s_2 <- lambda1[j+1] * ((n_2 + delta) / (n_2))
          u_2 <- S_func(m_2, s_2)

        } else if (n_2 == 0){
          u_2 <- 0
        }
        m_diff <- abs(u_1 - u_2)
        if(n_1*n_2 > 0){
          con_term <- (n_1 + n_2)/(n_1*n_2)
        } else {
          con_term <- 0
        }

        diff_vals <- m_diff/con_term


        l2_clus_boot[k] <- max(diff_vals)
      }
      l2_vec_boot[i] <- max(l2_clus_boot, na.rm = TRUE)
    }
    l2 <- unname(stats::quantile(l2_vec_boot, alpha2))
    l2_vec[j] <- l2
  }
  l2_calc <- rep(mean(l2_vec), dim-1)


  return(l2_calc)
}
