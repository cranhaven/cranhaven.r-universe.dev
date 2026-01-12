#' @title
#' Estimate the standard deviation (noise) of the nugget term in nnGP
#'
#' @description
#' Estimates the standard deviations of the nugget term (noise) in nnGP by
#' calculating the standard deviations of the residuals.
#'
#' @param hyperparam A vector of hyper-parameter values.
#' @param w_obs A vector of observed exposure levels.
#' @param GPS_obs A vector of estimated GPS evaluated at the observed exposure
#' levels.
#' @param y_obs A vector of observed outcomes.
#' @param n_neighbor A number of nearest neighbors on one side.
#' @param nthread A number of cores used in the estimation.
#' @param kernel_fn The covariance function of the GP.
#'
#' @return
#' A scalar of estimated standard deviation of the nugget term in nnGP.
#'
#' @keywords internal
#'
estimate_noise_nn <- function(hyperparam,
                              w_obs,
                              GPS_obs,
                              y_obs,
                              n_neighbor,
                              nthread,
                              kernel_fn = function(x) exp(-x ^ 2)) {

  t_1 <- proc.time()
  logger::log_info("Working on estimating residual error with nngp ...")

  alpha <- hyperparam[[1]]
  beta <- hyperparam[[2]]
  g_sigma <- hyperparam[[3]]

  obs <- cbind(w_obs * sqrt(1 / alpha), GPS_obs * sqrt(1 / beta))
  obs_ord <- obs[order(w_obs), ]
  y_ord <- y_obs[order(w_obs)]

  lfp <- get_options("logger_file_path")
  cl <- parallel::makeCluster(nthread, type = "PSOCK",
                              outfile = lfp)
  parallel::clusterExport(cl = cl,
                          varlist = c("w_obs", "obs_ord", "y_ord",
                                      "n_neighbor", "kernel_fn", "g_sigma",
                                      "arma_mm"),
                          envir = environment())

  all_residuals <- parallel::parSapply(cl,
                                       1:length(w_obs),
                                       function(i) {
    i_min <- max(i - n_neighbor / 2, 1)
    if (i_min - 1 + n_neighbor >= length(w_obs)) {
      idx_use <- (length(w_obs) - n_neighbor + 1):(length(w_obs))
    } else {
      idx_use <- i_min:(i_min + n_neighbor - 1)
    }

    cov_all <- g_sigma * kernel_fn(as.matrix(dist(obs_ord[c(i, idx_use), ]))) +
               diag(n_neighbor + 1)
    w <- c(arma_mm(compute_inverse(cov_all[-1, -1]), cov_all[1, -1]))
    c(w %*% y_ord[idx_use]) - y_ord[i]
  })
  parallel::stopCluster(cl)

  t_2 <- proc.time()
  logger::log_info("Done with estimating residual error with nngp",
                   "Wall clock time: {t_2[[3]] - t_1[[3]]} s.")

  return(sd(all_residuals))
}
