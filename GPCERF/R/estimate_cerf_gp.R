#' @title
#' Estimate the conditional exposure response function using Gaussian process
#'
#' @description
#' Estimates the conditional exposure response function (cerf) using Gaussian
#' Process (gp). The function tune the best match (the lowest covariate balance)
#' for the provided set of hyperparameters.
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
#' alpha, beta, and g_sigma can be a vector of parameters.
#' @param outcome_col An outcome column name in `data`.
#' @param treatment_col A treatment column name in `data`.
#' @param covariates_col Covariates columns name in `data`.
#' @param nthread An integer value that represents the number of threads to be
#' used by internal packages.
#' @param kernel_fn A kernel function. A default value is a Gaussian Kernel.
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
#' A cerf_gp object that includes the following values:
#'  - w, the vector of exposure levels.
#'  - pst_mean, Computed mean for the w vector.
#'  - pst_sd, Computed credible interval for the w vector.
#'
#' @export
#'
#' @examples
#' \donttest{
#' set.seed(129)
#' data <- generate_synthetic_data(sample_size = 100, gps_spec = 3)
#'
#'
#' # Estimate GPS function
#' gps_m <- estimate_gps(cov_mt = data[,-(1:2)],
#'                       w_all = data$treat,
#'                       sl_lib = c("SL.xgboost"),
#'                       dnorm_log = FALSE)
#'
#' # exposure values
#' w_all <- seq(0,10,1)
#'
#'
#' cerf_gp_obj <- estimate_cerf_gp(data,
#'                                 w_all,
#'                                 gps_m,
#'                                 params = list(alpha = c(0.1),
#'                                               beta=0.2,
#'                                               g_sigma = 1,
#'                                               tune_app = "all"),
#'                                 outcome_col = "Y",
#'                                 treatment_col = "treat",
#'                                 covariates_col = paste0("cf", seq(1,6)),
#'                                 nthread = 1)
#' }
#'
estimate_cerf_gp <- function(data, w, gps_m, params,
                             outcome_col, treatment_col, covariates_col,
                             nthread = 1, kernel_fn = function(x) exp(-x ^ 2)) {

  # Log system info
  log_system_info()

  # timing the function
  st_time_gp <- proc.time()

  # function call
  fcall <- match.call()

  # Double-check input parameters ----------------------------------------------
  if (any(is.na(data))) {
    stop("At this time, data with missing values is not supported.")
  }

  if (!is.data.frame(data)) {
    stop(paste0("Data should be a data.frame. ",
                "Current format: ", class(data)[1]))
  }

  if (!inherits(gps_m, "gps")) {
    stop(paste0("The gps_m should be a gps class. ",
                "Current format: ", class(gps_m)[1]))
  }

  if (nrow(data) != length(gps_m$gps$w)) {
    stop(paste0("Provided Data and GPS object should have the same size. ",
                "Current sizes: ", nrow(data), " vs ", length(gps_m$gps$w)))
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

  check_params(c("alpha", "beta", "g_sigma", "tune_app"), params)

  # Note: In the package, alpha is used to scale w and beta is used to scale
  # GPS following the same convention in the method paper.

  # TODO: Check values of parameters, too.

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

  logger::log_trace("Number of provided combination of tune parameters: ",
                    "{nrow(tune_params)}")

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
               " is not supported."))
  }

  logger::log_trace("Number of selected combination of tune parameters: ",
                    "{nrow(tune_params_subset)}")

  # Compute m, "confidence interval", and covariate balance for provided
  # hyperparameters. -----------------------------------------------------------
  if (nthread > 1 && nrow(tune_params_subset) > 1) {

    logger::log_trace("Starting a cluster with {nthread} threads ...")

    lfp <- get_options("logger_file_path")

    # make a cluster
    cl <- parallel::makeCluster(nthread, type = "PSOCK",
                                outfile = lfp)

    # export variables and functions to cluster cores
    parallel::clusterExport(cl = cl,
                            varlist = c("w", "outcome_data",
                                        "treatment_data",
                                        "covariates_data",
                                        "gps_m",
                                        "tune_params_subset",
                                        "kernel_fn",
                                        "compute_m_sigma"),
                            envir = environment())

    tune_res <- parallel::parApply(cl, tune_params_subset, 1,
                                   function(x) {
                                     compute_m_sigma(
                                       hyperparam = x,
                                       outcome_data =  outcome_data,
                                       treatment_data = treatment_data,
                                       covariates_data = covariates_data,
                                       w = w,
                                       gps_m = gps_m,
                                       tuning = TRUE,
                                       kernel_fn = kernel_fn)
                                   })

    parallel::stopCluster(cl)
    logger::log_trace("Cluster with {nthread} threads was terminated.")
  } else if (nrow(tune_params_subset) > 1) {
    tune_res <- apply(tune_params_subset, 1, function(x) {
      compute_m_sigma(hyperparam = x,
                      outcome_data =  outcome_data,
                      treatment_data = treatment_data,
                      covariates_data = covariates_data,
                      w = w,
                      gps_m = gps_m,
                      tuning = TRUE,
                      kernel_fn = kernel_fn)
      })
  }

  # Tuning results include:
  # cb: covariate balance for each confounder. This is the average of all
  #     all covariate balance for each requested exposure values.
  # est: Posterior mean for all requested exposure values (vector of
  #      NA if tuning).
  # pst: Posterior standard deviation for all requested exposure values (vector
  #.     of NA if tuning.)


  # Select the combination of hyperparameters that provides the lowest
  # covariate balance ----------------------------------------------------------
  if (nrow(tune_params_subset) > 1) {
    logger::log_debug("Number of generated tuning results: {length(tune_res)}")
    opt_idx <- order(sapply(tune_res, function(x) {mean(x$cb)}))[1]
  } else {
    opt_idx <- 1
  }
  opt_param <- tune_params_subset[opt_idx, ]
  gp_cerf_final <- compute_m_sigma(hyperparam = opt_param,
                                   outcome_data =  outcome_data,
                                   treatment_data = treatment_data,
                                   covariates_data = covariates_data,
                                   w = w,
                                   gps_m = gps_m,
                                   tuning = FALSE,
                                   kernel_fn = kernel_fn)


  # Build gp_cerf S3 object ----------------------------------------------------
  result <- list()
  class(result) <- "cerf_gp"

  # Hyper parameters ------------------------
  optimal_params <- opt_param
  names(optimal_params) <- c("alpha", "beta", "g_sigma")
  result$optimal_params <- optimal_params
  result$num_of_trial <- nrow(tune_params_subset)

  # Data ------------------------------------
  posterior <- list()
  posterior$mean <- gp_cerf_final$est
  posterior$sd <- gp_cerf_final$pst
  posterior$w <- w
  result$posterior <- posterior

  # Covariate balance -----------------------
  result$cb <- gp_cerf_final$cb
  result$cb_org <- gp_cerf_final$cb_org

  # Function call ---------------------------
  result$fcall <- fcall

  et_time_gp <- proc.time()
  logger::log_debug("Wall clock time to run estimate_cerf_gp:",
                    " {(et_time_gp -   st_time_gp)[[3]]} seconds.")

  # return gp_cerf S3 object
  invisible(result)
}
