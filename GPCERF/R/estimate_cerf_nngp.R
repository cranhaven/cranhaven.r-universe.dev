#' @title
#' Estimate the conditional exposure response function using nearest neighbor
#' Gaussian process
#'
#' @description
#' Estimates the conditional exposure response function (cerf) using
#' the nearest neighbor (nn) Gaussian Process (gp). The function tune the best
#' match (the lowest covariate balance) for the provided set of hyperparameters.
#'
#' @param data A data.frame of observation data.
#' @param w A vector of exposure level to compute CERF (please also see the
#' notes).
#' @param gps_m An S3 gps object including:
#'   gps: A data.frame of GPS vectors.
#'     - Column 1: GPS
#'     - Column 2: Prediction of exposure for covariate of each data sample
#'     (e_gps_pred).
#'     - Column 3: Standard deviation of  e_gps (e_gps_std)
#'   used_params:
#'     - dnorm_log: TRUE or FLASE
#' @param params A list of parameters that is required to run the process.
#' These parameters include:
#'   - alpha: A scaling factor for the GPS value.
#'   - beta: A scaling factor for the exposure value.
#'   - g_sigma: A scaling factor for kernel function (gamma/sigma).
#'   - tune_app: A tuning approach. Available approaches:
#'     - all: try all combinations of hyperparameters.
#'   - n_neighbor: Number of nearest neighbors on one side.
#'   - block_size: Number of samples included in a computation block. Mainly
#'   used to balance the speed and memory requirement. Larger \code{block_size}
#'   is faster, but requires more memory.
#' alpha, beta, and g_sigma can be a vector of parameters.
#' @param outcome_col An outcome column name in `data`.
#' @param treatment_col A treatment column name in `data`.
#' @param covariates_col Covariates columns name in `data`.
#' @param kernel_fn A kernel function. A default value is a Gaussian Kernel.
#' @param nthread An integer value that represents the number of threads to be
#' used by internal packages.
#'
#' @note
#' Please note that `w` is a vector representing a grid of exposure levels at
#' which the CERF is to be estimated. This grid can include both observed and
#' hypothetical values of the exposure variable. The purpose of defining this
#' grid is to provide a structured set of points across the exposure spectrum
#' for estimating the CERF. This approach is essential in nonparametric models
#' like Gaussian Processes (GPs), where the CERF is evaluated at specific points
#' to understand the relationship between the exposure and outcome variables
#' across a continuum. It facilitates a comprehensive analysis by allowing
#' practitioners to examine the effect of varying exposure levels, including
#' those not directly observed in the dataset.
#'
#' @return
#' A cerf_nngp object that includes the following values:
#'  - w, the vector of exposure levels.
#'  - pst_mean, the computed mean for the w vector.
#'  - pst_sd, the computed credible interval for the w vector.
#'
#' @export
#'
#' @examples
#'
#' \donttest{
#' set.seed(19)
#' data <- generate_synthetic_data(sample_size = 120, gps_spec = 3)
#' # Estimate GPS function
#' gps_m <- estimate_gps(cov_mt = data[,-(1:2)],
#'                       w_all = data$treat,
#'                       sl_lib = c("SL.xgboost"),
#'                       dnorm_log = FALSE)
#' # exposure values
#' w.all <- seq(0,20,2)
#' cerf_nngp_obj <- estimate_cerf_nngp(data,
#'                                     w.all,
#'                                     gps_m,
#'                                     params = list(alpha = c(0.1),
#'                                                   beta = 0.2,
#'                                                   g_sigma = 1,
#'                                                   tune_app = "all",
#'                                                   n_neighbor = 20,
#'                                                   block_size = 1e4),
#'                                     outcome_col = "Y",
#'                                     treatment_col = "treat",
#'                                     covariates_col = paste0("cf", seq(1,6)),
#'                                     nthread = 1)
#'}
#'
estimate_cerf_nngp <- function(data, w, gps_m, params,
                               outcome_col, treatment_col, covariates_col,
                               kernel_fn = function(x) exp(-x ^ 2),
                               nthread = 1) {

  # Log system info
  log_system_info()

  # function call
  fcall <- match.call()

  t_nngp_1 <- proc.time()
  logger::log_info("Working on estimating cerf using nngp approach ...")

  # Double-check input parameters ----------------------------------------------
  if (any(is.na(data))) {
    stop("At this time, data with missing values is not supported.")
  }

  check_params <- function(my_param, params) {
    for (item in my_param) {
      if (!is.element(c(item), names(params))) {
        stop(paste0("The required parameter, ", item, ", is not provided. ",
                    "Current parameters: ", paste(unlist(names(params)),
                                                  collapse = ", ")))
      }
    }
  }

  check_params(c("alpha", "beta", "g_sigma",
                 "tune_app", "n_neighbor", "block_size"), params)

  if (!inherits(gps_m, "gps")) {
    stop(paste0("The gps_m should be a gps class. ",
                "Current format: ", class(gps_m)[1]))
  }

  if (nrow(data) != length(gps_m$gps$w)) {
    stop(paste0("Provided Data and GPS object should have the same size.",
                "Current sizes: ", nrow(data), " vs ", length(gps_m$gps$w)))
  }

  # TODO: Check values of parameters, too.

  # Order data based on w ------------------------------------------------------
  data <- data[order(data[[treatment_col]]), ]
  gps_m$gps <- gps_m$gps[order(gps_m$gps$w), ]

  if (!all.equal(data[[treatment_col]], gps_m$gps$w, tolerance = 0.00001)) {
    stop(paste0("Provided GPS object and data object have different",
                " exposure values."))
  }

  # Collect data ---------------------------------------------------------------
  outcome_data <- data[[outcome_col]]
  treatment_data <- data[[treatment_col]]
  covariates_data <- data[, covariates_col, drop=FALSE]

  # Check if outcome_data and treatment_data are vectors
  is_outcome_vector <- is.vector(outcome_data) &&
                       !is.data.frame(outcome_data)

  is_treatment_vector <- is.vector(treatment_data) &&
                       !is.data.frame(treatment_data)
  # Check if covariates_data is a data.frame
  is_covariates_dataframe <- is.data.frame(covariates_data)

  if (!is_outcome_vector) {
    stop("outcome_data is not a vector.")
  }
  if (!is_treatment_vector) {
    stop("treatment_data is not a vector.")
  }
  if (!is_covariates_dataframe) {
    stop("covariates_data is not a data.frame.")
  }

  # Expand the grid of parameters (alpha, beta, g_sigma) -----------------------
  tune_params <-  expand.grid(getElement(params, "alpha"),
                              getElement(params, "beta"),
                              getElement(params, "g_sigma"))

  # Note: In the package, alpha is used to scale w and beta is used to scale
  # GPS following the same convention in the method paper.


  if (nrow(tune_params) == 0) {
    stop(paste("Something went wrong with tuning parameters. ",
               "The expanded grid has not been generated."))
  }

  # Choose subset of tuning parameters based on tuning approach ----------------

  if (getElement(params, "tune_app") == "all") {
    tune_params_subset <- tune_params
  } else if (getElement(params, "tune_app") == "at_random") {
    stop("This approach is not implemented.")
  } else {
    stop(paste("The provided tune_app approach, ",
               getElement(params, "tune_app"),
               ", is not supported."))
  }

  # Get other parameters -------------------------------------------------------
  n_neighbor <- getElement(params, "n_neighbor")
  block_size <- getElement(params, "block_size")

  # Search for the best set of parameters --------------------------------------
  optimal_cb_res <- find_optimal_nn(w_obs = treatment_data,
                                    w = w,
                                    y_obs = outcome_data,
                                    gps_m = gps_m,
                                    design_mt = covariates_data,
                                    hyperparams = tune_params_subset,
                                    n_neighbor = n_neighbor,
                                    kernel_fn = kernel_fn,
                                    block_size = block_size,
                                    nthread = nthread)

  # Extract the optimum hyperparameters
  all_cb_res <- sapply(optimal_cb_res, "[[", "cb")

  if (!is.matrix(all_cb_res)){
    # in case of one covariate col_all returns vector instead of matrix
    row_name <- names(all_cb_res)[1]
    all_cb_res <- matrix(all_cb_res, nrow = 1)
    rownames(all_cb_res) <- row_name
  }

  if (nrow(all_cb_res) == 1){
    opt_idx_nn <- order(abs(all_cb_res))[1]
  } else {
    opt_idx_nn <- order(colMeans(abs(all_cb_res)))[1]
  }

  posterior_mean <- optimal_cb_res[[opt_idx_nn]]$est
  nn_opt_param <- unlist(tune_params_subset[opt_idx_nn, ])

  # Estimate noise -------------------------------------------------------------
  noise_nn <- estimate_noise_nn(hyperparam = nn_opt_param,
                                w_obs = treatment_data,
                                GPS_obs = gps_m$gps$gps,
                                y_obs = outcome_data,
                                kernel_fn = kernel_fn,
                                n_neighbor = n_neighbor,
                                nthread = nthread)

  # Compute posterior mean and standard deviation ------------------------------
  posterior_vals <- estimate_mean_sd_nn(hyperparam = nn_opt_param,
                                        sigma2 = noise_nn ^ 2,
                                        w_obs = treatment_data,
                                        w = w,
                                        y_obs = outcome_data,
                                        gps_m = gps_m,
                                        kernel_fn = kernel_fn,
                                        n_neighbor = n_neighbor,
                                        block_size = block_size,
                                        nthread = nthread)

  posterior_sd <- posterior_vals

  t_nngp_2 <- proc.time()
  logger::log_info("Done with estimating cerf using nngp approach ",
                   "Wall clock time: {t_nngp_2[[3]] - t_nngp_1[[3]]} s.")

  # Compute covariate balance for original data --------------------------------
  cov_balance_obj_org <- compute_w_corr(w = treatment_data,
                                        covariate = covariates_data,
                                        weight = rep(1, nrow(data)))
  cb_org <- cov_balance_obj_org$absolute_corr


  # Build nngp_cerf S3 object --------------------------------------------------
  result <- list()
  class(result) <- "cerf_nngp"

  # Hyper parameterss -------------------------
  optimal_params <- nn_opt_param
  names(optimal_params) <- c("alpha", "beta", "g_sigma")
  result$optimal_params <- optimal_params
  result$num_of_trial <- nrow(tune_params_subset)

  # Data --------------------------------------
  posterior <- list()
  posterior$mean <- posterior_mean
  posterior$sd <- posterior_sd
  posterior$w <- w
  result$posterior <- posterior

  # Covariate balance -------------------------
  result$cb <- optimal_cb_res[[opt_idx_nn]]$cb
  result$cb_org <- cb_org

  # Function call -----------------------------
  result$fcall <- fcall

  t_nngp_2 <- proc.time()
  logger::log_debug("Wall clock time to run estimate_cerf_nngp:",
                    " {(t_nngp_2 -   t_nngp_1)[[3]]} seconds.")

  invisible(result)
}
