#' @title
#' Find the optimal hyper-parameter for the nearest neighbor Gaussian process
#'
#' @description
#' Computes covariate balance for each combination of provided hyper-parameters
#' and selects the hyper-parameter values that minimizes the covariate balance.
#'
#' @param w_obs A vector of the observed exposure levels.
#' @param w A vector of exposure levels at which CERF will be estimated.
#' @param y_obs A vector of observed outcomes
#' @param gps_m An S3 gps object including:
#'   gps: A data.frame of GPS vectors.
#'     - Column 1: GPS
#'     - Column 2: Prediction of exposure for covariate of each data sample
#'     (e_gps_pred).
#'     - Column 3: Standard deviation of  e_gps (e_gps_std)
#'   used_params:
#'     - dnorm_log: TRUE or FLASE
#' @param design_mt The covariate matrix of all samples (intercept excluded).
#' @param hyperparams A matrix of candidate values of the hyper-parameters,
#' each row contains a set of values of all hyper-parameters.
#' @param kernel_fn The covariance function of the GP.
#' @param n_neighbor The number of nearest neighbors on one side.
#' @param block_size The number of samples included in a computation block.
#' Mainly used to balance the speed and memory requirement. Larger
#' \code{block_size} is faster, but requires more memory.
#' @param nthread An integer value that represents the number of threads to be
#' used by internal packages.
#'
#' @return
#' Estimated covariate balance scores for the grid of hyper-parameter values
#' considered in \code{hyperparams}.
#'
#' @keywords internal
#'
find_optimal_nn <- function(w_obs, w, y_obs, gps_m, design_mt,
                            hyperparams = expand.grid(seq(0.5, 4.5, 1),
                                                      seq(0.5, 4.5, 1),
                                                      seq(0.5, 4.5, 1)),
                            kernel_fn = function(x) exp(-x^2),
                            n_neighbor = 50, block_size = 2e3,
                            nthread = 1) {

  logger::log_info("Started finding optimal values ... ")
  t_opt_1 <- proc.time()

  coord_obs <- cbind(w_obs, gps_m$gps$gps)

  lfp <- get_options("logger_file_path")

  # make a cluster
  t_cl_1 <- proc.time()
  cl <- parallel::makeCluster(nthread, type = "PSOCK",
                              outfile = lfp)

  # export variables and functions to cluster cores
  parallel::clusterExport(cl = cl,
                          varlist = c("w", "gps_m",
                                      "coord_obs", "y_obs", "kernel_fn",
                                      "n_neighbor", "block_size",
                                      "compute_posterior_m_nn",
                                      "compute_w_corr"),
                          envir = environment())

  t_cl_2 <- proc.time()

  logger::log_debug("Time to setup cluster with {nthread} core(s):",
                   "{t_cl_2[[3]] - t_cl_1[[3]]} s.")

  all_res <- apply(hyperparams, 1, function(hyperparam) {

    parallel::clusterExport(cl = cl,
                            varlist = c("hyperparam"),
                            envir = environment())


    all_res_list <- parallel::parLapply(cl,
                                        w,
                                        function(wi) {

      # Estimate GPS for requested w.
      gps_w <- dnorm(wi,
                     mean = gps_m$gps$e_gps_pred,
                     sd = gps_m$gps$e_gps_std,
                     log = gps_m$used_params$dnorm_log)

      # Compute posterior mean
      res <- compute_posterior_m_nn(hyperparam = hyperparam,
                                    w = wi,
                                    gps_w = gps_w,
                                    obs_ord = coord_obs,
                                    y_obs_ord = y_obs,
                                    n_neighbor = n_neighbor,
                                    kernel_fn = kernel_fn,
                                    block_size = block_size)
      idx <- res[-nrow(res), 1]
      weights <- res[-nrow(res), 2]
      cb_obj <- compute_w_corr(w = coord_obs[idx, 1],
                               covariate = design_mt[idx, ,drop=FALSE],
                               weight = weights)
      cb <- cb_obj$absolute_corr
      list(cb = cb, est = res[nrow(res), 2])
    })

    all_cb_tmp <- do.call(cbind, lapply(all_res_list, "[[", "cb"))
    all_est_tmp <- sapply(all_res_list, "[[", "est")

    #covariate specific balance, averaged over w
    list(cb = rowMeans(all_cb_tmp, na.rm = TRUE),
         est = all_est_tmp)
  })

  parallel::stopCluster(cl)

  t_opt_2 <- proc.time()
  logger::log_info("Done with finding optimal value.",
                   "(Wall clock time: {t_opt_2[[3]] - t_opt_1[[3]]} s.} ... ")


  return(all_res)
}
