#' Gap Statistic Calculator
#'
#' This function calculates the gap statistic for SparseMDC. For use
#' when the number of clusters in the data is unknown. We recommend
#' using alternate methods to infer the number of clusters in the
#' data.
#'
#' @param pdat list with D entries, each entry contains data d, p * n matrix.
#' This data should be centered and log-transformed.
#' @param dim Total number of conditions, D.
#' @param min_clus The minimum number of clusters to try, minimum value is 2.
#' @param max_clus The maximum number of clusters to try.
#' @param nboots The number of bootstrap repetitions to use, default = 200.
#' @param nitter The max number of iterations for each of the start values, the
#' default value is 20.
#' @param nstarts The number of start values to use for SparseDC. The default
#' value is 10.
#' @param l1_boot The number of bootstrap repetitions used for estimating
#' lambda 1.
#' @param l2_boot The number of bootstrap repetitions used for estimating
#' lambda 2.
#' @return A list containing the optimal number of clusters, as well as gap
#' statistics and the calculated standard error for each number of clusters.
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
#' # Run with one bootstrap sample for example
#' gap_stat <- sparsemdc_gap(pdat, dim=3, min_clus = 2, max_clus =3, nboots =2,
#' nitter = 2, nstarts = 1, l1_boot = 5, l2_boot = 5)
#'
sparsemdc_gap <- function(pdat, dim, min_clus, max_clus,
                          nboots = 200, nitter = 20, nstarts = 10,
                          l1_boot = 50, l2_boot = 50) {
  clus_seq <- c(min_clus:(max_clus+1))
  clus_score_vec <- clus_gap_vec <- clus_se_vec <- rep(NA, length(clus_seq))
  score_gap_vec <- score_se <- rep(NA, length(clus_seq))
  for( i in 1:length(clus_seq)){
    l1 <- lambda1_calculator(pdat, dim, nclust = clus_seq[i], nboot = l1_boot)
    l2 <- lambda2_calculator(pdat, dim , nclust = clus_seq[i], nboot = l2_boot,
                             lambda1 = l1)
    invisible(utils::capture.output(fit <- sparse_mdc(pdat, nclust = clus_seq[i], dim , lambda1 = l1,
                      lambda2 = l2, nstarts, nitter)))
    dist_total <- 0

    clus_score_vec[i] <- min(fit[[3]])
    boot_score_vals <- boot_score_vals_s <- rep(NA, nboots)
    for ( j in 1:nboots){
      set.seed(j)
      pdat_b <- pdat
      for(d in 1:dim){
        pdat_b[[d]] <- generate_uni_dat(pdat[[d]])

      }
      l1_b <- lambda1_calculator(pdat_b, dim, nclust = clus_seq[i], nboot = l1_boot)
      l2_b <- lambda2_calculator(pdat_b, dim , nclust = clus_seq[i], nboot = l2_boot,
                                 lambda1 = l1_b)
      invisible(utils::capture.output(fit_b <- sparse_mdc(pdat_b, nclust = clus_seq[i], dim , lambda1 = l1_b,
                          lambda2 = l2_b)))

      boot_score_vals[j] <- min(fit_b[[3]])
    }
    clus_gap_vec[i] <- (1/nboots)*sum(log(boot_score_vals)) -
      log(clus_score_vec[i])
    clus_se_vec[i] <- stats::sd(log(boot_score_vals)) * sqrt(1 + 1/nboots)

  }
  diff_vec <- (clus_gap_vec ) - clus_se_vec
  test_vec <- rep(NA, length(clus_seq) - 1)
  for(l in 1:(length(clus_seq) - 1)){
    test_vec[l] <- clus_gap_vec[l] > diff_vec[l + 1]
  }

  if (sum(test_vec == TRUE) == 0){
    print("The gap statistic could not find a solution")
  } else {
    gap_clus <- which(test_vec == TRUE)[1]
  }
  gap_clus_res <- clus_seq[gap_clus]
  return(list(k = gap_clus_res, gap_stat = clus_gap_vec, gap_se = clus_se_vec))
}

#'
#' Uniform data generator
#' For use with the gap statistic. Generates datasets drawn from the reference
#' distribution where each reference feature is generated uniformly over the
#' range of observed values for that feature.
#'
#' @param data A dataset with rows as features and columns as samples.
#' @return A dataset drawn from the reference distribution for use internally
#' with the gap statistic.
#'
generate_uni_dat <- function(data){
  min_vals <- apply(data, 1, min)
  max_vals <- apply(data, 1, max)
  uniform_dat <- matrix(stats::runif(length(data), min = min_vals, max = max_vals),
                        ncol= ncol(data))
  return(uniform_dat)
}
