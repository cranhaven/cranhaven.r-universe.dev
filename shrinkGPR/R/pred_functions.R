#' Evaluate Predictive Densities
#'
#' \code{eval_pred_dens} evaluates the predictive density for a set of points based on a fitted \code{shrinkGPR} model.
#'
#' @param x Numeric vector of points for which the predictive density is to be evaluated.
#' @param mod A \code{shrinkGPR} object representing the fitted Gaussian process regression model.
#' @param data_test Data frame with one row containing the covariates for the test set.
#' Variables in \code{data_test} must match those used in model fitting.
#' @param nsamp Positive integer specifying the number of posterior samples to use for the evaluation. Default is 100.
#' @param log Logical; if \code{TRUE}, returns the log predictive density. Default is \code{FALSE}.
#' @return A numeric vector containing the predictive densities (or log predictive densities) for the points in \code{x}.
#' @details
#' This function computes predictive densities by marginalizing over posterior samples drawn from the fitted model. If the mean equation is included in the model, the corresponding covariates are incorporated.
#' @examples
#' \donttest{
#' if (torch::torch_is_installed()) {
#'   # Simulate data
#'   set.seed(123)
#'   torch::torch_manual_seed(123)
#'   n <- 100
#'   x <- matrix(runif(n * 2), n, 2)
#'   y <- sin(2 * pi * x[, 1]) + rnorm(n, sd = 0.1)
#'   data <- data.frame(y = y, x1 = x[, 1], x2 = x[, 2])
#'
#'   # Fit GPR model
#'   res <- shrinkGPR(y ~ x1 + x2, data = data)
#'
#'   # Create point at which to evaluate predictive density
#'   data_test <- data.frame(x1 = 0.8, x2 = 0.5)
#'   eval_points <- c(-1.2, -1, 0)
#'
#'   eval_pred_dens(eval_points, res, data_test)
#'
#'   # Is vectorized, can also be used in functions like curve
#'   curve(eval_pred_dens(x, res, data_test), from = -1.5, to = -0.5)
#'   abline(v = sin(2 * pi * 0.8), col = "red")
#'   }
#' }
#' @export
eval_pred_dens <- function(x, mod, data_test, nsamp = 100, log = FALSE){

  # Input checking for eval_pred_dens --------------------------------------

  # Check that x is numeric
  if (!is.numeric(x)) {
    stop("The argument 'x' must be a numeric vector.")
  }

  # Check that mod is a shrinkGPR object
  if (!class(mod) %in% c("shrinkGPR", "shrinkTPR")) {
    stop("The argument 'mod' must be an object of class 'shrinkGPR' or 'shrinkTPR'.")
  }

  # Check that data_test is a data frame with one row
  if (!is.data.frame(data_test) || nrow(data_test) != 1) {
    stop("The argument 'data_test' must be a data frame with exactly one row.")
  }

  # Check that nsamp is a positive integer
  if (!is.numeric(nsamp) || nsamp <= 0 || nsamp %% 1 != 0) {
    stop("The argument 'nsamp' must be a positive integer.")
  }

  # Check that log is a logical value
  if (!is.logical(log) || length(log) != 1) {
    stop("The argument 'log' must be a single logical value.")
  }

  device <- attr(mod, "device")

  terms <- delete.response(mod$model_internals$terms)
  m <- model.frame(terms, data = data_test, xlev = mod$model_internals$xlevels)
  x_test <- torch_tensor(model.matrix(terms, m), device = device)

  if (mod$model_internals$x_mean) {
    terms_mean <- delete.response(mod$model_internals$terms_mean)
    m_mean <- model.frame(terms_mean, data = data_test, xlev = mod$model_internals$xlevels_mean)
    x_test_mean <- torch_tensor(model.matrix(terms_mean, m_mean), device = device)
  } else {
    x_test_mean <- NULL
  }

  x_tens <- torch_tensor(x, device = device)

  res_tens <- mod$model$eval_pred_dens(x_tens, x_test, nsamp, x_test_mean, log)
  return(as.numeric(res_tens))
}

#' Log Predictive Density Score
#'
#' \code{LPDS} calculates the log predictive density score for a fitted \code{shrinkGPR} model using a test dataset.
#'
#' @param mod A \code{shrinkGPR} object representing the fitted Gaussian process regression model.
#' @param data_test Data frame with one row containing the covariates for the test set.
#' Variables in \code{data_test} must match those used in model fitting.
#' @param nsamp Positive integer specifying the number of posterior samples to use for the evaluation. Default is 100.
#' @return A numeric value representing the log predictive density score for the test dataset.
#' @details
#' The log predictive density score is a measure of model fit that evaluates how well the model predicts unseen data.
#' It is computed as the log of the marginal predictive density of the observed responses.
#' @examples
#' \donttest{
#' if (torch::torch_is_installed()) {
#'   # Simulate data
#'   set.seed(123)
#'   torch::torch_manual_seed(123)
#'   n <- 100
#'   x <- matrix(runif(n * 2), n, 2)
#'   y <- sin(2 * pi * x[, 1]) + rnorm(n, sd = 0.1)
#'   data <- data.frame(y = y, x1 = x[, 1], x2 = x[, 2])
#'
#'   # Fit GPR model
#'   res <- shrinkGPR(y ~ x1 + x2, data = data)
#'
#'   # Calculate true y value and calculate LPDS at specific point
#'   x1_new <- 0.8
#'   x2_new <- 0.5
#'   y_true <- sin(2 * pi * x1_new)
#'   data_test <- data.frame(y = y_true, x1 = x1_new, x2 = x2_new)
#'   LPDS(res, data_test)
#'   }
#' }
#' @export
LPDS <- function(mod, data_test, nsamp = 100) {

  # Input checking for LPDS -------------------------------------------------

  # Check that mod is a shrinkGPR object
  if (!class(mod) %in% c("shrinkGPR", "shrinkTPR")) {
    stop("The argument 'mod' must be an object of class 'shrinkGPR' or 'shrinkTPR'.")
  }
  # Check that data_test is a data frame with one row
  if (!is.data.frame(data_test) || nrow(data_test) != 1) {
    stop("The argument 'data_test' must be a data frame with exactly one row.")
  }

  # Check that nsamp is a positive integer
  if (!is.numeric(nsamp) || nsamp <= 0 || nsamp %% 1 != 0) {
    stop("The argument 'nsamp' must be a positive integer.")
  }

  # Create Vector y
  terms <- mod$model_internals$terms
  m <- model.frame(terms, data = data_test, xlev = mod$model_internals$xlevels)
  y <- model.response(m, "numeric")

  eval_pred_dens(y, mod, data_test, nsamp, log = TRUE)
}

#' Calculate Predictive Moments
#'
#' \code{calc_pred_moments} calculates the predictive means and variances for a fitted \code{shrinkGPR} model at new data points.
#'
#' @param object A \code{shrinkGPR} object representing the fitted Gaussian process regression model.
#' @param newdata \emph{Optional} data frame containing the covariates for the new data points. If missing, the training data is used.
#' @param nsamp Positive integer specifying the number of posterior samples to use for the calculation. Default is 100.
#' @return A list with two elements:
#' \itemize{
#'   \item \code{means}: A matrix of predictive means for each new data point, with the rows being the samples and the columns the data points.
#'   \item \code{vars}: An array of covariance matrices, with the first dimension corresponding to the samples and second and third dimensions to the data points.
#' }
#' @details
#' This function computes predictive moments by marginalizing over posterior samples from the fitted model. If the mean equation is included in the model, the corresponding covariates are used.
#' @examples
#' \donttest{
#' if (torch::torch_is_installed()) {
#'   # Simulate data
#'   set.seed(123)
#'   torch::torch_manual_seed(123)
#'   n <- 100
#'   x <- matrix(runif(n * 2), n, 2)
#'   y <- sin(2 * pi * x[, 1]) + rnorm(n, sd = 0.1)
#'   data <- data.frame(y = y, x1 = x[, 1], x2 = x[, 2])
#'
#'   # Fit GPR model
#'   res <- shrinkGPR(y ~ x1 + x2, data = data)
#'
#'   # Calculate predictive moments
#'   momes <- calc_pred_moments(res, nsamp = 100)
#'   }
#' }
#' @export
calc_pred_moments <- function(object, newdata, nsamp = 100) {

  # Input checking for calc_pred_moments ------------------------------------

  # Check that object is a shrinkGPR object
  if (!class(object) %in% c("shrinkGPR", "shrinkTPR")) {
    stop("The argument 'object' must be an object of class 'shrinkGPR' or 'shrinkTPR'.")
  }

  # Check that newdata, if provided, is a data frame
  if (!missing(newdata) && !is.data.frame(newdata)) {
    stop("The argument 'newdata', if provided, must be a data frame.")
  }

  # Check that nsamp is a positive integer
  if (!is.numeric(nsamp) || nsamp <= 0 || nsamp %% 1 != 0) {
    stop("The argument 'nsamp' must be a positive integer.")
  }

  if (missing(newdata)) {
    newdata <- object$model_internals$data
  }

  device <- attr(object, "device")

  terms <- delete.response(object$model_internals$terms)
  m <- model.frame(terms, data = newdata, xlev = object$model_internals$xlevels)
  x_tens <- torch_tensor(model.matrix(terms, m), device = device)

  if (object$model_internals$x_mean) {
    terms_mean <- delete.response(object$model_internals$terms_mean)
    m_mean <- model.frame(terms_mean, data = newdata, xlev = object$model_internals$xlevels_mean)
    x_tens_mean <- torch_tensor(model.matrix(terms_mean, m_mean), device = device)
  } else {
    x_tens_mean <- NULL
  }

  res_tens <- object$model$calc_pred_moments(x_tens, nsamp, x_tens_mean)

  return(list(means = as.matrix(res_tens[[1]]),
              vars = as.array(res_tens[[2]])))
}

#' Generate Predictions
#'
#' \code{predict.shrinkGPR} generates posterior predictive samples from a fitted \code{shrinkGPR} model at specified covariates.
#'
#' @param object A \code{shrinkGPR} object representing the fitted Gaussian process regression model.
#' @param newdata \emph{Optional} data frame containing the covariates for the prediction points. If missing, the training data is used.
#' @param nsamp Positive integer specifying the number of posterior samples to generate. Default is 100.
#' @param ... Currently ignored.
#' @return A matrix containing posterior predictive samples for each covariate combination in \code{newdata}.
#' @details
#' This function generates predictions by sampling from the posterior predictive distribution. If the mean equation is included in the model, the corresponding covariates are incorporated.
#' @examples
#' \donttest{
#' if (torch::torch_is_installed()) {
#'   # Simulate data
#'   set.seed(123)
#'   torch::torch_manual_seed(123)
#'   n <- 100
#'   x <- matrix(runif(n * 2), n, 2)
#'   y <- sin(2 * pi * x[, 1]) + rnorm(n, sd = 0.1)
#'   data <- data.frame(y = y, x1 = x[, 1], x2 = x[, 2])
#'
#'   # Fit GPR model
#'   res <- shrinkGPR(y ~ x1 + x2, data = data)
#'   # Example usage for in-sample prediction
#'   preds <- predict(res)
#'
#'   # Example usage for out-of-sample prediction
#'   newdata <- data.frame(x1 = runif(10), x2 = runif(10))
#'   preds <- predict(res, newdata = newdata)
#'   }
#' }
#' @export
predict.shrinkGPR <- function(object, newdata, nsamp = 100, ...) {

  # Input checking for predict.shrinkGPR ------------------------------------

  # Check that mod is a shrinkGPR object
  if (!class(object) %in% c("shrinkGPR", "shrinkTPR")) {
    stop("The argument 'object' must be an object of class 'shrinkGPR' or 'shrinkTPR'.")
  }

  # Check that newdata, if provided, is a data frame
  if (!missing(newdata) && !is.data.frame(newdata)) {
    stop("The argument 'newdata', if provided, must be a data frame.")
  }

  # Check that nsamp is a positive integer
  if (!is.numeric(nsamp) || nsamp <= 0 || nsamp %% 1 != 0) {
    stop("The argument 'nsamp' must be a positive integer.")
  }

  if (missing(newdata)) {
    newdata <- object$model_internals$data
  }

  device <- attr(object, "device")

  terms <- delete.response(object$model_internals$terms)
  m <- model.frame(terms, data = newdata, xlev = object$model_internals$xlevels)
  x_tens <- torch_tensor(model.matrix(terms, m), device = device)

  if (object$model_internals$x_mean) {
    terms_mean <- delete.response(object$model_internals$terms_mean)
    m_mean <- model.frame(terms_mean, data = newdata, xlev = object$model_internals$xlevels_mean)
    x_tens_mean <- torch_tensor(model.matrix(terms_mean, m_mean), device = device)
  } else {
    x_tens_mean <- NULL
  }

  res_tens <- object$model$predict(x_tens, nsamp, x_tens_mean)

  return(as.matrix(res_tens))
}

#' Generate Predictions
#'
#' \code{predict.shrinkTPR} generates posterior predictive samples from a fitted \code{shrinkGPR} model at specified covariates.
#'
#' @param object A \code{shrinkTPR} object representing the fitted Gaussian process regression model.
#' @param newdata \emph{Optional} data frame containing the covariates for the prediction points. If missing, the training data is used.
#' @param nsamp Positive integer specifying the number of posterior samples to generate. Default is 100.
#' @param ... Currently ignored.
#' @return A matrix containing posterior predictive samples for each covariate combination in \code{newdata}.
#' @details
#' This function generates predictions by sampling from the posterior predictive distribution. If the mean equation is included in the model, the corresponding covariates are incorporated.
#' @examples
#' \donttest{
#' if (torch::torch_is_installed()) {
#'   # Simulate data
#'   set.seed(123)
#'   torch::torch_manual_seed(123)
#'   n <- 100
#'   x <- matrix(runif(n * 2), n, 2)
#'   y <- sin(2 * pi * x[, 1]) + rnorm(n, sd = 0.1)
#'   data <- data.frame(y = y, x1 = x[, 1], x2 = x[, 2])
#'
#'   # Fit GPR model
#'   res <- shrinkGPR(y ~ x1 + x2, data = data)
#'   # Example usage for in-sample prediction
#'   preds <- predict(res)
#'
#'   # Example usage for out-of-sample prediction
#'   newdata <- data.frame(x1 = runif(10), x2 = runif(10))
#'   preds <- predict(res, newdata = newdata)
#'   }
#' }
#' @export
predict.shrinkTPR <- function(object, newdata, nsamp = 100, ...) {
  predict.shrinkGPR(object, newdata, nsamp, ...)
}

#' Generate Posterior Samples
#'
#' \code{gen_posterior_samples} generates posterior samples of the model parameters from a fitted \code{shrinkGPR} or \code{shrinkTPR} model.
#'
#' @param mod A \code{shrinkGPR} object representing the fitted Gaussian process regression model.
#' @param nsamp Positive integer specifying the number of posterior samples to generate. Default is 1000.
#' @return A list containing posterior samples of the model parameters:
#' \itemize{
#'   \item \code{thetas}: A matrix of posterior samples for the inverse lengthscale parameters.
#'   \item \code{sigma2}: A matrix of posterior samples for the noise variance.
#'   \item \code{lambda}: A matrix of posterior samples for the global shrinkage parameter.
#'   \item \code{betas} (optional): A matrix of posterior samples for the mean equation parameters (if included in the model).
#'   \item \code{lambda_mean} (optional): A matrix of posterior samples for the mean equation's global shrinkage parameter (if included in the model).
#' }
#' @details
#' This function draws posterior samples from the latent space and transforms them into the parameter space of the model. These samples can be used for posterior inference or further analysis.
#' @examples
#' \donttest{
#' if (torch::torch_is_installed()) {
#'   # Simulate data
#'   set.seed(123)
#'   torch::torch_manual_seed(123)
#'   n <- 100
#'   x <- matrix(runif(n * 2), n, 2)
#'   y <- sin(2 * pi * x[, 1]) + rnorm(n, sd = 0.1)
#'   data <- data.frame(y = y, x1 = x[, 1], x2 = x[, 2])
#'
#'   # Fit GPR model
#'   res <- shrinkGPR(y ~ x1 + x2, data = data)
#'
#'   # Generate posterior samples
#'   samps <- gen_posterior_samples(res, nsamp = 1000)
#'
#'   # Plot the posterior samples
#'   boxplot(samps$thetas)
#'   }
#' }
#' @export
gen_posterior_samples <- function(mod, nsamp = 1000) {

  # Input checking for gen_posterior_samples -------------------------------

  # Check that mod is a shrinkGPR object
  if (!class(mod) %in% c("shrinkGPR", "shrinkTPR")) {
    stop("The argument 'mod' must be an object of class 'shrinkGPR' or 'shrinkTPR'.")
  }

  # Check that nsamp is a positive integer
  if (!is.numeric(nsamp) || nsamp <= 0 || nsamp %% 1 != 0) {
    stop("The argument 'nsamp' must be a positive integer.")
  }

  with_no_grad({
    z <- mod$model$gen_batch(nsamp)
    zk <- mod$model(z)[[1]]
  })

  # Split into list containing groups of parameters
  # Convention:
  # First d_cov components are the theta parameters
  # Next component is the sigma parameter
  # Next component is the lambda parameter
  # Next d_mean components are the mean parameters
  # Last component is the lambda parameter for the mean

  d_cov <- mod$model_internals$d_cov

  res <- list(thetas = as.matrix(zk[, 1:d_cov]),
              sigma2 = as.matrix(zk[, d_cov + 1]),
              lambda = as.matrix(zk[, d_cov + 2]))

  colnames(res$thetas) <- paste0("theta_", attr(mod$model_internals$terms, "term.labels"))


  if (mod$model_internals$x_mean) {
    d_mean <- mod$model_internals$d_mean
    res$betas <- as.matrix(zk[, (d_cov + 3):(d_cov + 2 + d_mean)])
    res$lambda_mean <- as.matrix(zk[, d_cov + 2 + d_mean + 1])

    colnames(res$betas) <- paste0("beta_", mod$model_internals$x_mean_names)
  }

  if (inherits(mod, "shrinkTPR")) {
    res$nu <- as.matrix(zk[, -1]) + 2
  }

  return(res)


}


#' Generate Marginal Samples of Predictive Distribution
#'
#' \code{gen_marginal_samples()} generates model predictions over a grid of values for one or two specified covariates,
#' while filling in the remaining covariates either by drawing from the training data (if \code{fixed_x} is not provided)
#' or by using a fixed values for the remaining covariates (if \code{fixed_x} is provided). The result is a set of conditional
#' predictions that can be used to visualize the marginal effect of the selected covariates under varying input configurations.
#'
#' @param mod A \code{shrinkGPR} or \code{shrinkTPR} object representing the fitted Gaussian/t process regression model.
#' @param to_eval A character vector specifying the names of the covariates to evaluate. Can be one or two variables.
#' @param nsamp Positive integer specifying the number of posterior samples to generate. Default is 200.
#' @param fixed_x \emph{optional} data frame specifying a fixed covariate configuration. If provided, this configuration is used for
#'  all nonswept covariates. If omitted, covariates are sampled from the training data.
#' @param n_eval_points Positive integer specifying the number of evaluation points along each axis. If missing, defaults to 100
#' for 1D and 30 for 2D evaluations.
#' @param eval_range \emph{optional} numeric vector (1D) or list of two numeric vectors (2D) specifying the range over which to evaluate
#' the covariates in \code{to_eval}. If omitted, the range is set to the range of the swept covariate(s) in the training data.
#' @param display_progress logical value indicating whether to display progress bars and messages during training. The default is \code{TRUE}.
#' @return A list containing posterior predictive summaries over the evaluation grid:
#' \itemize{
#'   \item \code{mean_pred}: A matrix (1D case) or array (2D case) of predicted means for each evaluation point and posterior sample.
#'   \item \code{grid}: The evaluation grid used to generate predictions. A numeric vector (1D) or a named list of two vectors (\code{grid1}, \code{grid2}) for 2D evaluations.
#' }
#' @details
#' This function generates conditional predictive surfaces by evaluating the fitted model across a grid of values for one or two selected covariates.
#' For each of the \code{nsamp} draws, the remaining covariates are either held fixed (if \code{fixed_x} is provided) or filled in by sampling a single row from the training data.
#' The selected covariates in \code{to_eval} are then varied across a regular grid defined by \code{n_eval_points} and \code{eval_range}, and model predictions are computed using \code{\link{calc_pred_moments}}.
#'
#' The resulting samples represent conditional predictions across different covariate contexts, and can be used to visualize marginal effects, interaction surfaces, or predictive uncertainty.
#'
#' Note that computational and memory requirements increase rapidly with grid size. In particular, for two-dimensional evaluations, the
#' kernel matrix scales quadratically with the number of evaluation points per axis. Large values of \code{n_eval_points} may lead to high
#' memory usage during prediction, especially when using a GPU. If memory constraints arise, consider reducing \code{n_eval_points}.
#' @examples
#' \donttest{
#' if (torch::torch_is_installed()) {
#'   # Simulate data
#'   set.seed(123)
#'   torch::torch_manual_seed(123)
#'   n <- 100
#'   x <- matrix(runif(n * 2), n, 2)
#'   y <- sin(2 * pi * x[, 1]) + rnorm(n, sd = 0.1)
#'   data <- data.frame(y = y, x1 = x[, 1], x2 = x[, 2])
#'
#'   # Fit GPR model
#'   res <- shrinkGPR(y ~ x1 + x2, data = data)
#'
#'   # Generate posterior samples
#'   samps <- gen_posterior_samples(res, nsamp = 1000)
#'
#'   # Plot the posterior samples
#'   boxplot(samps$thetas)
#'   }
#' }
#' @export
gen_marginal_samples <- function(mod, to_eval, nsamp = 200, fixed_x, n_eval_points, eval_range, display_progress = TRUE) {

  # Input checking ----------------------------------------------------------

  # Check that mod is a supported model object
  if (!inherits(mod, c("shrinkGPR", "shrinkTPR"))) {
    stop("The argument 'mod' must be a 'shrinkGPR' or 'shrinkTPR' object.")
  }

  # Check that to_eval is a character vector of length 1 or 2
  if (!is.character(to_eval) || !(length(to_eval) %in% c(1, 2))) {
    stop("The argument 'to_eval' must be a character vector of length 1 or 2.")
  }

  # Check that to_eval are valid covariate names in the model
  valid_covariates <- attr(mod$model_internals$terms, "term.labels")
  if (!all(to_eval %in% valid_covariates)) {
    stop("The covariates specified in 'to_eval' must be names present in the colnames of the model's data.")
  }

  # Check nsamp is a positive integer
  if (int_input_bad(nsamp)) {
    stop("The argument 'nsamp' must be a positive integer.")
  }

  # Check display_progress is a logical scalar
  if (bool_input_bad(display_progress)) {
    stop("The argument 'display_progress' must be a single logical value.")
  }

  # Check fixed_x (if provided) is a data frame
  if (!missing(fixed_x) && !is.data.frame(fixed_x)) {
    stop("The argument 'fixed_x', if provided, must be a data frame.")
  }

  # Check n_eval_points (if provided) is a positive integer
  if (!missing(n_eval_points) && int_input_bad(n_eval_points)) {
    stop("The argument 'n_eval_points', if provided, must be a positive integer.")
  }

  # Check eval_range (if provided)
  if (!missing(eval_range)) {
    if (length(to_eval) == 1) {
      if (!is.numeric(eval_range) || length(eval_range) != 2 || any(is.na(eval_range))) {
        stop("For 1D evaluation, 'eval_range' must be a numeric vector of length 2.")
      }
    } else if (length(to_eval) == 2) {
      if (!is.list(eval_range) || length(eval_range) != 2 ||
          !all(sapply(eval_range, function(x) is.numeric(x) && length(x) == 2 && all(!is.na(x))))) {
        stop("For 2D evaluation, 'eval_range' must be a list of two numeric vectors, each of length 2.")
      }
    }
  }

  # If missing n_eval_points, set defaults based on to_eval length
  # 100 for 1D grid, 30 for 2D grid, as not to overwhelm memory
  if (missing(n_eval_points)) {
    if (length(to_eval) == 1) {
      n_eval_points <- 100
    } else if (length(to_eval) == 2) {
      n_eval_points <- 30
    }
  }

  # Set default eval_range to range of swept covariates, if user did not specify otherwise
  if (missing(eval_range)) {
    if (length(to_eval) == 1) {
      eval_range <- range(mod$model_internals$data[[to_eval]], na.rm = TRUE)
    } else if (length(to_eval) == 2) {
      eval_range <- list(
        eval_range1 = range(mod$model_internals$data[[to_eval[1]]], na.rm = TRUE),
        eval_range2 = range(mod$model_internals$data[[to_eval[2]]], na.rm = TRUE)
      )
    }
  }

  if (length(to_eval) == 1) {

    # 1D case

    # Generate grid of points to evaluate as well as storage object for samples
    grid <- seq(eval_range[1], eval_range[2], length.out = n_eval_points)
    samples <- matrix(NA, nrow = nsamp, ncol = n_eval_points)

    # Set up progress bar
    if (display_progress) {
      pb <- progress_bar$new(total = nsamp, format = "[:bar] :percent :eta",
                             clear = FALSE, width = 100)
    }

    # Loop over number of samples
    for (i in 1:nsamp) {

      if (missing(fixed_x)) {
        # Create synthetic data by sampling from the model's data
        index <- sample(1:nrow(mod$model_internals$data), 1)
        curr_data <- mod$model_internals$data[index, ][rep(1, n_eval_points), ]
      } else {
        # Use fixed_x to create synthetic data
        curr_data <- fixed_x
      }

      # Replicate single row
      curr_data <- curr_data[rep(1, n_eval_points), ]

      # Replace column of interest with grid values
      curr_data[, to_eval] <- grid


      samples[i, ] <- calc_pred_moments(mod, newdata = curr_data, nsamp = 1)[[1]]

      if (display_progress) {
        pb$tick()
      }


    }

    res <- list(mean_pred = samples, grid = grid)
    attr(res, "class") <- "shrinkGPR_marg_samples_1D"
    attr(res, "to_eval") <- to_eval
    attr(res, "response") <- as.character(mod$model_internals$terms[[2]])

    return(res)

  } else if (length(to_eval) == 2) {

    # 2D case

    # Create grid as well as container to hold samples
    grid1 <- seq(eval_range[[1]][1], eval_range[[1]][2], length.out = n_eval_points)
    grid2 <- seq(eval_range[[2]][1], eval_range[[2]][2], length.out = n_eval_points)
    grid_tot <- expand.grid(grid1, grid2)

    samples <- array(NA, dim = c(nsamp, n_eval_points, n_eval_points))

    if (display_progress) {
      pb <- progress_bar$new(total = nsamp, format = "[:bar] :percent :eta",
                             clear = FALSE, width = 100)
    }

    for (i in 1:nsamp) {

      if (missing(fixed_x)) {
        # Create synthetic data by sampling from the model's data
        index <- sample(1:nrow(mod$model_internals$data), 1)
        curr_data <- mod$model_internals$data[index, ]
      } else {
        # Use fixed_x to create synthetic data
        curr_data <- fixed_x
      }

      # Replicate single row
      curr_data <- curr_data[rep(1, n_eval_points^2), ]

      # Replace columns of interest with grid values
      curr_data[, to_eval[1]] <- grid_tot[, 1]
      curr_data[, to_eval[2]] <- grid_tot[, 2]

      pred_moments <- calc_pred_moments(mod, newdata = curr_data, nsamp = 1)[[1]]

      samples[i, , ] <- matrix(pred_moments, nrow = n_eval_points, ncol = n_eval_points)

      if (display_progress) {
        pb$tick()
      }
    }

    res <- list(mean_pred = samples, grid = list(grid1 = grid1, grid2 = grid2))
    attr(res, "class") <- "shrinkGPR_marg_samples_2D"
    attr(res, "to_eval") <- to_eval
    attr(res, "response") <- as.character(mod$model_internals$terms[[2]])

    return(res)

  }
}

