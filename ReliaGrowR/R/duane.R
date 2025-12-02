#' Duane Analysis
#'
#' This function performs a Duane analysis (1962) <doi:10.1109/TA.1964.4319640>
#' on failure data by fitting a log-log linear regression of cumulative Mean Time
#' Between Failures (MTBF) versus cumulative time. The function accepts either
#' two numeric vectors (`times`, `failures`) or a data frame containing both.
#'
#' @srrstats {G1.0} The primary reference for Duane Analysis is provided in the
#' description.
#' @srrstats {G1.1} The `duane` function is the first implementation of the Duane
#' model within an R package on CRAN.
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} [roxygen2](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} Multivariate inputs are prohibited where only univariate are allowed.
#' @srrstats {G2.4b} Explicit conversion of log-likelihood to continuous is made via `as.numeric()`.
#' @srrstats {G2.6} One-dimensional inputs are appropriately pre-processed.
#' @srrstats {G2.7} Both one-dimensional vectors and data frames are accepted as input.
#' @srrstats {G2.8} Sub-functions `print.duane` and `plot.duane` are provided for the `duane` class.
#' @srrstats {G2.10} `data.frame` objects are appropriately processed to ensure consistent behavior.
#' @srrstats {G2.11} `data.frame` inputs  are appropriately processed and do not error without reason.
#' @srrstats {G2.13} The function checks for missing data and errors if any is found.
#' @srrstats {G2.14} See sub-tags for responses.
#' @srrstats {G2.14a} Missing data results in an error.
#' @srrstats {G2.14b} Missing data results in an error.
#' @srrstats {G2.14c} Missing data results in an error.
#' @srrstats {G2.15} The function checks for missing data and errors if any is found.
#' @srrstats {G2.16} The function checks for NA and NaN values and errors if any are found.
#' @srrstats {G5.0} The function is tested with a standard data set from a published paper.
#' @srrstats {G5.1} The function is tested with a standard data set. The data set is
#' created within and used to test the package. The data set is exported so that users
#' can confirm tests and run examples.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.2a} Every message produced by `stop()` is unique.
#' @srrstats {G5.2b} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.4} Unit tests include correctness tests to test that statistical algorithms produce expected results to some fixed test data sets.
#' @srrstats {G5.4c} Unit tests include stored values that are drawn from a published paper output.
#' @srrstats {G5.5} Correctness tests are run with a fixed random seed.
#' @srrstats {G5.6} Unit tests include parameter recovery checks to test that the implementation produce expected results given data with known properties.
#' @srrstats {G5.6a} Parameter recovery tests are expected to be within a defined tolerance rather than exact values.
#' @srrstats {G5.7} Unit tests include algorithm performance checks to test that the function performs as expected as data changes.
#' @srrstats {G5.8} See sub-tags for responses.
#' @srrstats {G5.8a} Unit tests include checks for zero-length data.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {G5.8c} Unit tests include checks for data with 'NA' fields.
#' @srrstats {G5.8d} Unit tests include checks for data outside the scope of the algorithm.
#' @srrstats {G5.9} Unit tests include noise susceptibility tests for expected stochastic behavior.
#' @srrstats {G5.9a} Unit tests check that adding trivial noise to data does not meaningfully change results.
#' @srrstats {G5.9b} Unit tests check that different random seeds do not meaningfully change results.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.
#' @srrstats {RE1.2} Documentation includes expected format for inputting predictor variables (`times`, `failures`).
#' @srrstats {RE1.3} Output retains all relevant aspects of input data.
#' @srrstats {RE1.3a} Output retains all relevant aspects of input data.
#' @srrstats {RE1.4} Documentation includes assumptions for the input data (i.e., positive, finite values).
#' @srrstats {RE2.1} NA, NaN, and Inf values in input data result in an error.
#' @srrstats {RE2.2} Missing values in input data results in an error.
#' @srrstats {RE4.0} Software returns a “model” object, which is a modified `lm` model object.
#' @srrstats {RE4.2} Model coefficients are included in the output object.
#' @srrstats {RE4.3} Standard errors on the coefficients are included in the output object.
#' @srrstats {RE4.4} The model specification is included in the output object.
#' @srrstats {RE4.5} The numbers of observations is included in the output object.
#' @srrstats {RE4.8} The Response variable (MTBF) is included in the output object.
#' @srrstats {RE4.9} Modeled values of the response variable are included in the output object.
#' @srrstats {RE4.10} Model Residuals, including documentation is included in the output object.
#' @srrstats {RE4.11} Goodness-of-fit statistics (log-likelihood, AIC, BIC) are included
#' in the output object.
#' @srrstats {RE4.13} All input variables are included in the output object.
#' @srrstats {RE4.17} Model objects are extended by a default `print` method which
#' provides an on-screen summary of model (input) parameters and (output) coefficients.
#' @srrstats {RE5.0} Scaling relationships between sizes of input data and
#' speed of algorithm are documented in the function documentation.
#' @srrstats {RE6.0} Model objects have default plot methods.
#' @srrstats {RE6.2} The default plot method produces a plot of the fitted values
#' of the model, with optional visualisation of confidence intervals.
#' @srrstats {RE7.1} Unit tests check for noiseless, exact relationships between
#' predictor (independent) and response (dependent) data.
#' @srrstats {RE7.1a} Unit tests confirm that model fitting is at least as fast
#' or faster than testing with equivalent noisy data.
#' @srrstats {RE7.2} Unit tests demonstrate that output objects retain aspects
#' of input data such as case names.
#' @srrstats {RE7.3} Unit tests demonstrate expected behavior when `duane` object
#' is submitted to the accessor methods `print` and `plot`.
#'
#' @param times Either:
#'   - A numeric vector of cumulative failure times, or
#'   - A data frame containing two columns: `times` and `failures`. The `times`
#'     column contains cumulative failure times, and the `failures` column
#'     contains the number of failures at each corresponding time.
#' @param failures A numeric vector of the number of failures at each corresponding
#'   time in `times`. Ignored if `times` is a data frame. Must be the same length as
#'   `times` if both are vectors.
#'   All values must be positive and finite.
#' @param conf.level Confidence level for the confidence bounds (default: `0.95`).
#'   Must be between 0 and 1 (exclusive).
#'
#' @family Duane functions
#'
#' @return A list of class \code{"duane"} containing:
#' \item{times}{The input cumulative failure times.}
#' \item{failures}{The input number of failures.}
#' \item{n_obs}{The number of observations (failures).}
#' \item{MTBF}{The cumulative mean time between failures.}
#' \item{model}{The fitted \code{lm} (linear model) object containing the regression results.}
#' \item{logLik}{The log-likelihood of the fitted model.}
#' \item{AIC}{Akaike Information Criterion (AIC).}
#' \item{BIC}{Bayesian Information Criterion (BIC).}
#' \item{conf.level}{The confidence level.}
#' \item{Cumulative_Time}{The cumulative operating times.}
#' \item{Cumulative_MTBF}{The cumulative mean time between failures.}
#' \item{Fitted_Values}{The fitted values on the MTBF scale.}
#' \item{Confidence_Bounds}{Matrix of fitted values and confidence bounds on the MTBF scale.}
#' \item{Residuals_Log}{Residuals on the log(MTBF) scale (from the regression).}
#' \item{Residuals_MTBF}{Residuals on the MTBF scale (observed - fitted).}
#'
#' @details
#' The scaling relationship between the size of input data (numbers of observations)
#' and speed of algorithm execution is approximately linear (O(n)). The function is
#' efficient and can handle large data sets (e.g., thousands of observations) quickly.
#' The function uses base R functions and does not require any additional packages.
#' The function includes comprehensive input validation and error handling to ensure
#' robustness. The function is tested with a standard data set from a published paper
#' and includes unit tests to verify correctness and performance.
#'
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' fit1 <- duane(times, failures, conf.level = 0.90)
#' print(fit1)
#'
#' df <- data.frame(times = times, failures = failures)
#' fit2 <- duane(df, conf.level = 0.95)
#' print(fit2)
#'
#' @importFrom stats lm AIC BIC logLik predict residuals
#' @export
duane <- function(times, failures = NULL, conf.level = 0.95) {
  # Case 1: data frame input
  if (is.data.frame(times)) {
    if (!all(c("times", "failures") %in% names(times))) {
      stop(
        "Data frame input must contain columns named 'times' and 'failures'."
      )
    }
    failures <- times$failures
    times <- times$times
  }

  # Input validation
  if (!is.numeric(times) || !is.vector(times)) {
    stop("'times' must be a numeric vector.")
  }
  if (!is.numeric(failures) || !is.vector(failures)) {
    stop("'failures' must be a numeric vector.")
  }
  if (any(is.na(times)) || any(is.nan(times))) {
    stop("'times' contains missing (NA) or NaN values.")
  }
  if (any(is.na(failures)) || any(is.nan(failures))) {
    stop("'failures' contains missing (NA) or NaN values.")
  }
  if (length(times) == 0) {
    stop("'times' cannot be empty.")
  }
  if (length(failures) == 0) {
    stop("'failures' cannot be empty.")
  }
  if (length(times) != length(failures)) {
    stop("The length of 'times' and 'failures' must be equal.")
  }
  if (any(!is.finite(times)) || any(times <= 0)) {
    stop("All values in 'times' must be finite and > 0.")
  }
  if (any(!is.finite(failures)) || any(failures <= 0)) {
    stop("All values in 'failures' must be finite and > 0.")
  }
  if (!is.numeric(conf.level) || length(conf.level) != 1) {
    stop("'conf.level' must be a single numeric value.")
  }
  if (conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' must be between 0 and 1 (exclusive).")
  }

  # Cumulative times & failures
  cum_failures <- cumsum(failures)
  cum_times <- cumsum(times)

  # cumulative MTBF
  cum_mtbf <- cum_times / cum_failures

  # log-log transform
  log_cum_times <- log(cum_times)
  log_cum_failures <- log(cum_failures)
  log_cum_mtbf <- log(cum_mtbf)

  # Fit model (log-log)
  fit <- stats::lm(log_cum_mtbf ~ log_cum_times)

  # fit stats
  aic <- stats::AIC(fit)
  bic <- stats::BIC(fit)
  loglik <- as.numeric(stats::logLik(fit))

  # Fitted values and residuals
  fitted_log <- predict(fit)
  fitted_values <- exp(fitted_log)
  residuals_log <- residuals(fit)
  residuals_mtbf <- cum_mtbf - fitted_values

  # confidence intervals on MTBF scale
  pred <- predict(fit, interval = "confidence", level = conf.level)
  conf_intervals <- stats::predict(fit, interval = "confidence", level = conf.level)
  ci_bounds <- exp(pred)
  lower_bounds <- exp(conf_intervals[, "lwr"])
  upper_bounds <- exp(conf_intervals[, "upr"])

  result <- list(
    times = times,
    failures = failures,
    n_obs = length(failures),
    MTBF = cum_mtbf,
    model = fit,
    logLik = loglik,
    AIC = aic,
    BIC = bic,
    conf.level = conf.level,
    Cumulative_Time = cum_times,
    Cumulative_MTBF = cum_mtbf,
    Fitted_Values = fitted_values,
    Confidence_Bounds = ci_bounds,
    lower_bounds = lower_bounds,
    upper_bounds = upper_bounds,
    Residuals_Log = residuals_log,
    Residuals_MTBF = residuals_mtbf
  )
  class(result) <- "duane"
  return(result)
}


#' Print method for duane objects.
#'
#' This function prints a summary of the Duane analysis result.
#'
#' @srrstats {G1.0} The primary reference for Duane Analysis is provided in the
#' description.
#' @srrstats {G1.1} The `duane` function is the first implementation of the Duane
#' model within an R package on CRAN.
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} [roxygen2](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} Multivariate inputs are prohibited where only univariate are allowed.
#' @srrstats {G2.4b} Explicit conversion of log-likelihood to continuous is made via `as.numeric()`.
#' @srrstats {G2.6} One-dimensional inputs are appropriately pre-processed.
#' @srrstats {G2.7} Both one-dimensional vectors and data frames are accepted as input.
#' @srrstats {G2.8} Sub-functions `print.duane` and `plot.duane` are provided for the `duane` class.
#' @srrstats {G2.10} `data.frame` objects are appropriately processed to ensure consistent behavior.
#' @srrstats {G2.11} `data.frame` inputs  are appropriately processed and do not error without reason.
#' @srrstats {G2.13} The function checks for missing data and errors if any is found.
#' @srrstats {G2.14} See sub-tags for responses.
#' @srrstats {G2.14a} Missing data results in an error.
#' @srrstats {G2.14b} Missing data results in an error.
#' @srrstats {G2.14c} Missing data results in an error.
#' @srrstats {G2.15} The function checks for missing data and errors if any is found.
#' @srrstats {G2.16} The function checks for NA and NaN values and errors if any are found.
#' @srrstats {G5.0} The function is tested with a standard data set from a published paper.
#' @srrstats {G5.1} The function is tested with a standard data set. The data set is
#' created within and used to test the package. The data set is exported so that users
#' can confirm tests and run examples.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.2a} Every message produced by `stop()` is unique.
#' @srrstats {G5.2b} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.4} Unit tests include correctness tests to test that statistical algorithms produce expected results to some fixed test data sets.
#' @srrstats {G5.4c} Unit tests include stored values that are drawn from a published paper output.
#' @srrstats {G5.5} Correctness tests are run with a fixed random seed.
#' @srrstats {G5.6} Unit tests include parameter recovery checks to test that the implementation produce expected results given data with known properties.
#' @srrstats {G5.6a} Parameter recovery tests are expected to be within a defined tolerance rather than exact values.
#' @srrstats {G5.7} Unit tests include algorithm performance checks to test that the function performs as expected as data changes.
#' @srrstats {G5.8} See sub-tags for responses.
#' @srrstats {G5.8a} Unit tests include checks for zero-length data.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {G5.8c} Unit tests include checks for data with 'NA' fields.
#' @srrstats {G5.8d} Unit tests include checks for data outside the scope of the algorithm.
#' @srrstats {G5.9} Unit tests include noise susceptibility tests for expected stochastic behavior.
#' @srrstats {G5.9a} Unit tests check that adding trivial noise to data does not meaningfully change results.
#' @srrstats {G5.9b} Unit tests check that different random seeds do not meaningfully change results.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.
#'
#' @param x An object of class "duane" returned by the duane_plot function.
#' @param ... Additional arguments (not used).
#' @family Duane functions
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- duane(times, failures)
#' print(fit)
#' @return Invisibly returns the input object.
#'
#' @export
print.duane <- function(x, ...) {
  # Input validation
  if (!inherits(x, "duane")) {
    stop("'x' must be an object of class 'duane'.")
  }

  cat("Duane Analysis Result\n")
  cat("----------------------\n")
  cat("Linear model (log-log scale): log(MTBF) ~ log(Time)\n")

  # Number of observations (failures)
  cat(sprintf("\nNumber of observations (failures): %d\n", x$n_obs))

  # Coefficient estimates and standard errors
  smry <- summary(x$model)
  coefs <- smry$coefficients

  cat("\nCoefficients:\n")
  print(coefs[, c("Estimate", "Std. Error")])

  # Model fit statistics
  cat(sprintf("\nLog-likelihood: %.2f", x$logLik))
  cat(sprintf("\nAIC: %.2f, BIC: %.2f", x$AIC, x$BIC))

  # Confidence level, if used
  if (!is.null(x$conf.level)) {
    cat(sprintf("\nConfidence level: %.1f%%", 100 * x$conf.level))
  }

  cat("\n")
  invisible(x)
}

#' Plot Method for Duane Analysis
#'
#' Generates a Duane plot (log-log or linear scale) with fitted regression line
#' and optional confidence bounds.
#'
#' @srrstats {G1.0} The primary reference for Duane Analysis is provided in the
#' description.
#' @srrstats {G1.1} The `duane` function is the first implementation of the Duane
#' model within an R package on CRAN.
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} [roxygen2](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} Multivariate inputs are prohibited where only univariate are allowed.
#' @srrstats {G2.4b} Explicit conversion of log-likelihood to continuous is made via `as.numeric()`.
#' @srrstats {G2.6} One-dimensional inputs are appropriately pre-processed.
#' @srrstats {G2.7} Both one-dimensional vectors and data frames are accepted as input.
#' @srrstats {G2.8} Sub-functions `print.duane` and `plot.duane` are provided for the `duane` class.
#' @srrstats {G2.10} `data.frame` objects are appropriately processed to ensure consistent behavior.
#' @srrstats {G2.11} `data.frame` inputs  are appropriately processed and do not error without reason.
#' @srrstats {G2.13} The function checks for missing data and errors if any is found.
#' @srrstats {G2.14} See sub-tags for responses.
#' @srrstats {G2.14a} Missing data results in an error.
#' @srrstats {G2.14b} Missing data results in an error.
#' @srrstats {G2.14c} Missing data results in an error.
#' @srrstats {G2.15} The function checks for missing data and errors if any is found.
#' @srrstats {G2.16} The function checks for NA and NaN values and errors if any are found.
#' @srrstats {G5.0} The function is tested with a standard data set from a published paper.
#' @srrstats {G5.1} The function is tested with a standard data set. The data set is
#' created within and used to test the package. The data set is exported so that users
#' can confirm tests and run examples.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.2a} Every message produced by `stop()` is unique.
#' @srrstats {G5.2b} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.4} Unit tests include correctness tests to test that statistical algorithms produce expected results to some fixed test data sets.
#' @srrstats {G5.4c} Unit tests include stored values that are drawn from a published paper output.
#' @srrstats {G5.5} Correctness tests are run with a fixed random seed.
#' @srrstats {G5.6} Unit tests include parameter recovery checks to test that the implementation produce expected results given data with known properties.
#' @srrstats {G5.6a} Parameter recovery tests are expected to be within a defined tolerance rather than exact values.
#' @srrstats {G5.7} Unit tests include algorithm performance checks to test that the function performs as expected as data changes.
#' @srrstats {G5.8} See sub-tags for responses.
#' @srrstats {G5.8a} Unit tests include checks for zero-length data.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {G5.8c} Unit tests include checks for data with 'NA' fields.
#' @srrstats {G5.8d} Unit tests include checks for data outside the scope of the algorithm.
#' @srrstats {G5.9} Unit tests include noise susceptibility tests for expected stochastic behavior.
#' @srrstats {G5.9a} Unit tests check that adding trivial noise to data does not meaningfully change results.
#' @srrstats {G5.9b} Unit tests check that different random seeds do not meaningfully change results.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.
#'
#' @param x An object of class \code{"duane"}.
#' @param log Logical; whether to use logarithmic scales for axes (default: \code{TRUE}).
#' @param conf.int Logical; whether to plot confidence bounds (default: \code{TRUE}).
#' @param legend Logical; whether to include a legend (default: TRUE).
#' @param legend.pos Position of the legend (default: "topleft").
#' @param ... Further arguments passed to \code{plot()}.
#' @family Duane functions
#'
#' @return Invisibly returns \code{NULL}.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- duane(times, failures)
#' plot(fit, main = "Duane Plot", xlab = "Cumulative Time", ylab = "Cumulative MTBF")
#' @importFrom graphics legend lines plot
#' @export
plot.duane <- function(
    x,
    log = TRUE,
    conf.int = TRUE,
    legend = TRUE,
    legend.pos = "topleft",
    ...) {
  # Input validation
  if (!inherits(x, "duane")) {
    stop("'x' must be an object of class 'duane'.")
  }
  if (!is.logical(log) || length(log) != 1) {
    stop("'log' must be a single logical value.")
  }
  if (!is.logical(conf.int) || length(conf.int) != 1) {
    stop("'conf.int' must be a single logical value.")
  }
  if (!is.logical(legend) || length(legend) != 1) {
    stop("'legend' must be a single logical value.")
  }
  if (!is.character(legend.pos) || length(legend.pos) != 1) {
    stop("'legend.pos' must be a single character string.")
  }

  cum_time <- x$Cumulative_Time
  cum_mtbf <- x$Cumulative_MTBF

  plot_args <- list(
    x = cum_time,
    y = cum_mtbf,
    pch = 16,
    ...
  )
  if (log) {
    plot_args$log <- "xy"
  }
  do.call(graphics::plot, plot_args)

  # fitted line
  graphics::lines(cum_time, x$Fitted_Values, lty = 1)

  # confidence bounds (optional)
  if (conf.int && !is.null(x$Confidence_Bounds)) {
    graphics::lines(cum_time, x$Confidence_Bounds[, "lwr"], lty = 2)
    graphics::lines(cum_time, x$Confidence_Bounds[, "upr"], lty = 2)
  }

  # legend
  if (legend) {
    legend_items <- c("Observed", "Fitted Line")
    pch <- c(16, NA)
    lty <- c(NA, 1)

    if (conf.int && !is.null(x$Confidence_Bounds)) {
      legend_items <- c(legend_items, "Confidence Bounds")
      pch <- c(pch, NA)
      lty <- c(lty, 2)
    }

    graphics::legend(
      legend.pos,
      legend = legend_items,
      pch = pch,
      lty = lty,
      bty = "n"
    )
  }

  invisible(NULL)
}
