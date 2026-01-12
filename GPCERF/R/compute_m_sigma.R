#' @title
#' Compute mean, credible interval, and covariate balance in standard Gaussian
#'  process (GP)
#'
#' @description
#' Calculates the induced covariate balance associated with one hyper-parameter
#' configuration in standard GP.
#'
#' @param hyperparam A vector of values of hyper-parameters.
#'   - First element: alpha
#'   - Second element: beta
#'   - Third element: g_sigma (gamma / sigma)
#' @param outcome_data A  vector of outcome data.
#' @param treatment_data A vector of treatment data.
#' @param covariates_data A data frame of covariates data.
#' @param w A vector of exposure levels at which the CERF is estimated.
#' @param gps_m An S3 gps object including:
#'   gps: A data.frame of GPS vectors.
#'     - Column 1: GPS
#'     - Column 2: Prediction of exposure for covariate of each data sample
#'     (e_gps_pred).
#'     - Column 3: Standard deviation of  e_gps (e_gps_std)
#'   used_params:
#'     - dnorm_log: TRUE or FLASE
#' @param tuning The function is used for parameter tuning (default = TRUE)
#' or estimation (FALSE)
#' @param kernel_fn The covariance function of GP.
#'
#' @return
#' A list containing two elements:
#'   - A vector of absolute weighted correlation of each covariate to the
#'   exposure, which is the metric for covariate balance
#'   - An estimated CERF at \code{w_all} based on the hyper-parameter values in
#'   \code{param}.
#'
#' @keywords internal
#'
compute_m_sigma <- function(hyperparam, outcome_data, treatment_data,
                            covariates_data, w, gps_m, tuning,
                            kernel_fn = function(x) exp(-x ^ 2)) {

  param <- unlist(hyperparam)

  gps <- gps_m$gps$gps

  # mi(w)
  # param 1: alpha
  # param 2: beta
  # param 3: ratio gamma/sigma

  alpha <- param[1]
  beta  <- param[2]
  g_sigma <- param[3]

  logger::log_trace("Should go through tuning? {tuning}")
  logger::log_trace("Running for tune parameters: ",
                    "alpha: {alpha}, beta: {beta}, g_sigma: {g_sigma} ...")

  w_obs <- treatment_data

  #TODO: Following the paper and alpha beta convention, first column should be
  # GPS scaled with alpha, and second column should be w scaled with beta.

  scaled_obs <- cbind(w_obs * sqrt(1 / beta), gps * sqrt(1 / alpha))
  colnames(scaled_obs) <- c("w_sc_obs", "gps_sc_obs")


  t_sigma_obs_1 <- proc.time()
  sigma_obs <- g_sigma * kernel_fn(as.matrix(dist(scaled_obs))) +
               diag(nrow(scaled_obs))
  t_sigma_obs_2 <- proc.time()

  logger::log_trace("Wall clock time to generate covariance matrix ",
                    "({nrow(sigma_obs)},{ncol(sigma_obs)}): ",
                    "{t_sigma_obs_2[[3]] - t_sigma_obs_1[[3]]} s.")


  inv_sigma_obs <- compute_inverse(sigma_obs)

  # Estimate noise
  if (!tuning) {
    noise_est <- estimate_noise_gp(data = outcome_data,
                                   sigma_obs = sigma_obs,
                                   inv_sigma_obs = inv_sigma_obs)
    logger::log_debug("Estimated noise: {noise_est} ")
  }

  logger::log_debug("Computing weight and covariate balance for each ",
                   "requested exposure value ... ")

  col_all_list <- lapply(w,
                         function(w_instance) {

    # compute weights
    weights_res <- compute_weight_gp(w = w_instance,
                                     w_obs = w_obs,
                                     scaled_obs = scaled_obs,
                                     hyperparam = hyperparam,
                                     inv_sigma_obs = inv_sigma_obs,
                                     gps_m = gps_m,
                                     est_sd = !tuning,
                                     kernel_fn = kernel_fn)

    weights_final <- weights_res$weight
    weights_final[weights_final < 0] <- 0
    if (sum(weights_final) > 0) {
      weights_final <- weights_final / sum(weights_final)
    }

    # weigts.final = invers of paranthesis * kappa
    # est is the same as m in the paper.

    if (!tuning) {
      est <- outcome_data %*% weights_final
      pst_sd <- noise_est * weights_res$sd_scaled
      logger::log_trace("Posterior for w = {w_instance} ==> ",
                        "mu: {est}, var:{pst_sd}")
    } else {
      est <- NA
      pst_sd <- NA
    }
    cov_balance_obj <- compute_w_corr(w = treatment_data,
                                      covariate = covariates_data,
                                      weight = weights_final)
    covariate_balance <- as.vector(cov_balance_obj$absolute_corr)
    c(covariate_balance, est, pst_sd)
    list(covariate_balance = cov_balance_obj,
         est = est,
         pst_sd = pst_sd)
  })

  logger::log_debug("Done with computing weight and covariate balance for ",
                   "each requested exposure value. ")

  col_all <- sapply(col_all_list, function(x) {
    x$covariate_balance$absolute_corr
    })
  est <- sapply(col_all_list, function(x) x$est)
  pst_sd <- sapply(col_all_list, function(x) x$pst_sd)

  # compute original covariate balance of data
  if (!tuning) {
    cov_balance_obj_org <- compute_w_corr(w = treatment_data,
                                          covariate = covariates_data,
                                          weight = rep(1, nrow(covariates_data)))
    cb_org <- cov_balance_obj_org$absolute_corr
  } else {
    cb_org <- NA
  }

  if (!is.matrix(col_all)){
    # in case of one covariate col_all returns vector instead of matrix
    row_name <- names(col_all)[1]
    col_all <- matrix(col_all, nrow = 1)
    rownames(col_all) <- row_name
  }


  if (nrow(col_all) == 1){
    col_all_w_average <- mean(col_all, na.rm = TRUE)
    names(col_all_w_average) <- rownames(col_all)
  } else {
    col_all_w_average <- rowMeans(col_all, na.rm = TRUE)
  }

  list(cb = col_all_w_average,
       cb_org = cb_org,
       est = est,
       pst = pst_sd)
}
