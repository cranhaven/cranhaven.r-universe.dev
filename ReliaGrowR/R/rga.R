#' Reliability Growth Analysis.
#'
#' This function performs reliability growth analysis using the Crow-AMSAA model by
#' Crow (1975) <https://apps.dtic.mil/sti/citations/ADA020296> or piecewise
#' NHPP model by Guo et al. (2010) <doi:10.1109/RAMS.2010.5448029>. It fits
#' a log-log linear regression of cumulative failures versus cumulative time. The
#' function accepts either two numeric vectors (`times`, `failures`) or a data frame
#' containing both. The `Piecewise NHPP` model can automatically detect change points
#' or use user-specified breakpoints.
#'
#' @srrstats {G1.0} Primary references for Crow-AMSAA and Piecewise NHPP models
#' are provided in the description.
#' @srrstats {G1.1} The `rga` function is the first implementation of the Crow-AMSAA
#'  and Piecewise NHPP models within an R package on CRAN.
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} `roxygen2`](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} Multivariate inputs are prohibited where only univariate are allowed.
#' @srrstats {G2.3} See sub-tags for responses.
#' @srrstats {G2.3a} `match.arg()` is used for string inputs.
#' @srrstats {G2.3b} `tolower()` is used for string inputs.
#' @srrstats {G2.4b} Explicit conversion of log-likelihood to continuous is made via `as.numeric()`.
#' @srrstats {G2.6} One-dimensional inputs are appropriately pre-processed.
#' @srrstats {G2.7} Both one-dimensional vectors and data frames are accepted as input.
#' @srrstats {G2.8} Sub-functions `print.rga` and `plot.rga` are provided for the `rga` class.
#' @srrstats {G2.10} Data extracted from tabular `data.frame` objects are checked to ensure consistent behavior.
#' @srrstats {G2.11} Unit tests check that `data.frame` inputs  are appropriately processed and do not error without reason.
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
#' @srrstats {G5.6} Unit tests include parameter recovery checks to test that the implementation produces expected results given data with known properties.
#' @srrstats {G5.6a} Parameter recovery tests are expected to be within a defined tolerance rather than exact values.
#' @srrstats {G5.7} Unit tests include algorithm performance checks to test that the function performs as expected as parameters change.
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
#' @srrstats {RE2.1} NA, NaN, and Inf values in input data results in an error.
#' @srrstats {RE2.2} Missing values in input data results in an error.
#' @srrstats {RE2.4} Function includes check for perfect collinearity between predictor and response variables.
#' @srrstats {RE2.4a} Perfect collinearity between predictor and response variables results in an error.
#' @srrstats {RE2.4b} Perfect collinearity between predictor and response variables results in an error.
#' @srrstats {RE4.0} Software returns a “model” object, which is a modified `lm` model object.
#' @srrstats {RE4.2} Model coefficients are included in the output object.
#' @srrstats {RE4.3} Standard errors on the coefficients are included in the output object.
#' @srrstats {RE4.4} The model specification is included in the output object.
#' @srrstats {RE4.5} The numbers of observations is included in the output object.
#' @srrstats {RE4.8} The Response variable (cumulative failures) is included in the output object.
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
#' @srrstats {RE7.3} Unit tests demonstrate expected behavior when `rga` object
#' is submitted to the accessor methods `print` and `plot`.
#'
#' @param times Either a numeric vector of cumulative failure times or a data frame
#' containing both failure times and failure counts. If a data frame is provided, it must
#' contain two columns: `times` and `failures`. The `times` column contains cumulative failure times,
#' and the `failures` column contains the number of failures at each corresponding time.
#' @param failures A numeric vector of the number of failures at each corresponding time
#' in times. Must be the same length as `times` if both are vectors. All values must be
#' positive and finite. Ignored if `times` is a data frame.
#' @param model_type The model type. Either `Crow-AMSAA` (default) or `Piecewise NHPP` with change point detection.
#' @param breaks An optional vector of breakpoints for the `Piecewise NHPP` model.
#' @param conf_level The desired confidence level, which defaults to 95%. The confidence
#' level is the probability that the confidence interval contains the true mean response.
#' @family Reliability Growth Analysis
#' @return The function returns an object of class `rga` that contains:
#' \item{times}{The input cumulative failure times.}
#' \item{failures}{The input number of failures.}
#' \item{n_obs}{The number of observations (failures).}
#' \item{cum_failures}{Cumulative failures.}
#' \item{model}{The fitted model object (lm (linear model) or segmented).}
#' \item{residuals}{Model residuals on the log-log scale. These represent deviations of the observed
#' log cumulative failures from the fitted values and are useful for diagnostic checking.}
#' \item{logLik}{The log-likelihood of the fitted model. The log-likelihood is a
#' measure of model fit, with higher values indicating a better fit.}
#' \item{AIC}{Akaike Information Criterion (AIC). AIC is a measure used for model selection,
#' with lower values indicating a better fit.}
#' \item{BIC}{Bayesian Information Criterion(BIC). BIC is another criterion for model selection}
#' \item{breakpoints}{Breakpoints (log scale) if applicable.}
#' \item{fitted_values}{Fitted cumulative failures on the original scale.}
#' \item{lower_bounds}{Lower confidence bounds (original scale).}
#' \item{upper_bounds}{Upper confidence bounds (original scale).}
#' \item{betas}{Estimated beta(s). Betas are the slopes of the log-log plot.}
#' \item{betas_se}{Standard error(s) of the estimated beta(s).}
#' \item{growth_rate}{Estimated growth rate(s). Growth rates are calculated as 1 - beta.}
#' \item{lambdas}{Estimated lambda(s). Lambdas are the intercepts of the log-log plot.}
#'
#' @details
#' The scaling relationship between the size of input data (numbers of observations)
#' and speed of algorithm execution is approximately linear (O(n)). The function is
#' efficient and can handle large data sets (e.g., thousands of observations) quickly.
#' The function uses the `segmented` package for piecewise regression, which employs
#' an iterative algorithm to estimate breakpoints. The number of iterations required
#' for convergence may vary depending on the data and initial values.
#' In practice, the function typically converges within a few iterations for most data sets.
#' However, in some cases, especially with complex data or poor initial values,
#' it may take more iterations.
#'
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result1 <- rga(times, failures)
#' print(result1)
#'
#' df <- data.frame(times = times, failures = failures)
#' result2 <- rga(df)
#' print(result2)
#'
#' result3 <- rga(times, failures, model_type = "Piecewise NHPP")
#' print(result3)
#'
#' result4 <- rga(times, failures, model_type = "Piecewise NHPP", breaks = c(450))
#' print(result4)
#' @importFrom stats lm predict AIC BIC logLik cor residuals
#' @importFrom segmented segmented slope intercept seg.control
#' @export



rga <- function(times, failures, model_type = "Crow-AMSAA", breaks = NULL, conf_level = 0.95) {
  if (is.data.frame(times)) {
    if (!all(c("times", "failures") %in% names(times))) {
      stop("If a data frame is provided, it must contain columns 'times' and 'failures'.")
    }
    failures <- times$failures
    times <- times$times
  }

  # Validation checks
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
  if (length(times) == 0) stop("'times' cannot be empty.")
  if (length(failures) == 0) stop("'failures' cannot be empty.")
  if (length(times) != length(failures)) {
    stop("The length of 'times' and 'failures' must be equal.")
  }
  if (any(!is.finite(times)) || any(times <= 0)) {
    stop("All values in 'times' must be finite and > 0.")
  }
  if (any(!is.finite(failures)) || any(failures <= 0)) {
    stop("All values in 'failures' must be finite and > 0.")
  }

  if (!is.character(model_type) || length(model_type) != 1) {
    stop("'model_type' must be a single character string.")
  }
  valid_model <- match.arg(
    tolower(model_type),
    tolower(c("crow-amsaa", "piecewise nhpp"))
  )

  if (!is.null(breaks)) {
    if (!is.numeric(breaks) || length(breaks) == 0) {
      stop("'breaks' must be a non-empty numeric vector if provided.")
    }
    if (any(!is.finite(breaks)) || any(breaks <= 0)) {
      stop("All values in 'breaks' must be finite and > 0.")
    }
    if (valid_model != "piecewise nhpp") {
      stop("'breaks' can only be used with the 'Piecewise NHPP' model.")
    }
  }

  if (!is.numeric(conf_level) || length(conf_level) != 1) {
    stop("'conf_level' must be a single numeric value.")
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be between 0 and 1 (exclusive).")
  }

  # Data prep
  cum_failures <- cumsum(failures)
  cum_time <- cumsum(times)
  log_times <- log(cum_time)
  log_cum_failures <- log(cum_failures)

  # Check for perfect collinearity
  cor_val <- suppressWarnings(stats::cor(log_times, log_cum_failures))
  if (is.na(cor_val) || abs(cor_val - 1) < .Machine$double.eps^0.5 ||
    abs(cor_val + 1) < .Machine$double.eps^0.5) {
    stop("Perfect collinearity detected between predictor ('log_times') and response ('log_cum_failures'). Regression cannot be performed.")
  }

  # Fit initial Crow-AMSAA model
  fit <- stats::lm(log_cum_failures ~ log_times)

  if (valid_model == "piecewise nhpp") {
    if (is.null(breaks)) {
      updated_fit <- segmented::segmented(fit, seg.Z = ~log_times)
      breakpoints <- updated_fit$psi[, "Est."]
    } else {
      breakpoints <- log(breaks)
      updated_fit <- segmented::segmented(fit, seg.Z = ~log_times, psi = breakpoints)
    }
    slopes <- segmented::slope(updated_fit)
    intercepts <- segmented::intercept(updated_fit)
    betas <- slopes
    growth_rates <- 1 - slopes$log_times[, "Est."]
    lambdas <- exp(intercepts$log_times)

    # Standard errors
    beta_se <- slopes$log_times[, "St.Err."]
  } else {
    updated_fit <- fit
    breakpoints <- NULL
    smry <- summary(updated_fit)
    slope <- smry$coefficients[2, ]
    intercept <- smry$coefficients[1, ]
    betas <- slope["Estimate"]
    growth_rates <- 1 - betas
    lambdas <- exp(intercept["Estimate"])

    # Standard Error
    beta_se <- slope["Std. Error"]
    lambdas_se <- exp(intercept["Std. Error"])
  }

  # Fit statistics
  loglik <- as.numeric(stats::logLik(updated_fit))
  aic <- stats::AIC(updated_fit)
  bic <- stats::BIC(updated_fit)

  # Predictions values
  fitted_values <- stats::predict(updated_fit)
  residuals <- stats::residuals(updated_fit)
  conf_intervals <- stats::predict(updated_fit, interval = "confidence", level = conf_level)
  lower_bounds <- exp(conf_intervals[, "lwr"])
  upper_bounds <- exp(conf_intervals[, "upr"])

  # Return object
  result <- list(
    times = times,
    failures = failures,
    n_obs = length(failures),
    cum_failures = cum_failures,
    model = updated_fit,
    residuals = residuals,
    logLik = loglik,
    AIC = aic,
    BIC = bic,
    breakpoints = breakpoints,
    fitted_values = exp(fitted_values),
    lower_bounds = lower_bounds,
    upper_bounds = upper_bounds,
    growth_rate = growth_rates,
    betas = betas,
    betas_se = beta_se,
    lambdas = lambdas
  )
  class(result) <- "rga"
  return(result)
}

#' Print method for rga objects.
#'
#' This function prints a summary of the results from an object of class \code{rga}.
#'
#' @srrstats {G1.0} Primary references for Crow-AMSAA and Piecewise NHPP models
#' are provided in the description.
#' @srrstats {G1.1} The `rga` function is the first implementation of the Crow-AMSAA
#'  and Piecewise NHPP models within an R package on CRAN.
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} `roxygen2`](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} Multivariate inputs are prohibited where only univariate are allowed.
#' @srrstats {G2.3} See sub-tags for responses.
#' @srrstats {G2.3a} `match.arg()` is used for string inputs.
#' @srrstats {G2.3b} `tolower()` is used for string inputs.
#' @srrstats {G2.4b} Explicit conversion of log-likelihood to continuous is made via `as.numeric()`.
#' @srrstats {G2.6} One-dimensional inputs are appropriately pre-processed.
#' @srrstats {G2.7} Both one-dimensional vectors and data frames are accepted as input.
#' @srrstats {G2.8} Sub-functions `print.rga` and `plot.rga` are provided for the `rga` class.
#' @srrstats {G2.10} Data extracted from tabular `data.frame` objects are checked to ensure consistent behavior.
#' @srrstats {G2.11} Unit tests check that `data.frame` inputs  are appropriately processed and do not error without reason.
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
#' @srrstats {G5.6} Unit tests include parameter recovery checks to test that the implementation produces expected results given data with known properties.
#' @srrstats {G5.6a} Parameter recovery tests are expected to be within a defined tolerance rather than exact values.
#' @srrstats {G5.7} Unit tests include algorithm performance checks to test that the function performs as expected as parameters change.
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
#' @param x An object of class \code{rga}, which contains the results from the RGA model.
#' @param ... Additional arguments (not used).
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' print(result)
#' @family Reliability Growth Analysis
#' @return Invisibly returns the input object.
#'
#' @export
print.rga <- function(x, ...) {
  # Input validation
  if (!inherits(x, "rga")) {
    stop("'x' must be an object of class 'rga'.")
  }

  cat("Reliability Growth Analysis (RGA)\n")
  cat("---------------------------------\n")

  model_type <- if (is.null(x$breakpoints)) "Crow-AMSAA" else "Piecewise NHPP"
  cat("Model Type:", model_type, "\n\n")

  if (!is.null(x$breakpoints)) {
    cat("Breakpoints (original scale):\n")
    cat(round(exp(x$breakpoints), 4), "\n\n")
  }

  # Number of observations (failures)
  cat(sprintf("\nNumber of observations (failures): %d\n", x$n_obs))

  cat("Parameters (per segment):\n")
  if (model_type == "Piecewise NHPP") {
    growth_rates <- round(x$growth_rate, 4)
    betas <- round(x$betas$log_times[, "Est."], 4)
    betas_se <- round(x$betas_se, 4)
    cat(sprintf("  Growth Rates: %s\n", paste(growth_rates, collapse = ", ")))
    cat(sprintf("  Betas: %s\n", paste(betas, collapse = ", ")))
    cat(sprintf("  Std. Errors (Betas): %s\n", paste(betas_se, collapse = ", ")))

    lambdas <- round(x$lambdas[, "Est."], 4)
    cat(sprintf("  Lambdas: %s\n", paste(lambdas, collapse = ", ")))
  } else {
    cat(sprintf("  Growth Rate: %.4f\n", x$growth_rate))
    cat(sprintf("  Beta: %.4f (SE = %.4f)\n", x$betas, x$betas_se))
    cat(sprintf("  Lambda: %.4f\n", x$lambdas))
  }

  cat("\nGoodness of Fit:\n")
  cat(sprintf("  Log-likelihood: %.2f\n", x$logLik))
  cat(sprintf("  AIC: %.2f\n", x$AIC))
  cat(sprintf("  BIC: %.2f\n", x$BIC))

  invisible(x)
}

#' Plot Method for RGA Objects
#'
#' This function generates plots for objects of class \code{rga}.
#'
#' @srrstats {G1.0} Primary references for Crow-AMSAA and Piecewise NHPP models
#' are provided in the description.
#' @srrstats {G1.1} The `rga` function is the first implementation of the Crow-AMSAA
#'  and Piecewise NHPP models within an R package on CRAN.
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} `roxygen2`](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} Multivariate inputs are prohibited where only univariate are allowed.
#' @srrstats {G2.3} See sub-tags for responses.
#' @srrstats {G2.3a} `match.arg()` is used for string inputs.
#' @srrstats {G2.3b} `tolower()` is used for string inputs.
#' @srrstats {G2.4b} Explicit conversion of log-likelihood to continuous is made via `as.numeric()`.
#' @srrstats {G2.6} One-dimensional inputs are appropriately pre-processed.
#' @srrstats {G2.7} Both one-dimensional vectors and data frames are accepted as input.
#' @srrstats {G2.8} Sub-functions `print.rga` and `plot.rga` are provided for the `rga` class.
#' @srrstats {G2.10} Data extracted from tabular `data.frame` objects are checked to ensure consistent behavior.
#' @srrstats {G2.11} Unit tests check that `data.frame` inputs  are appropriately processed and do not error without reason.
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
#' @srrstats {G5.6} Unit tests include parameter recovery checks to test that the implementation produces expected results given data with known properties.
#' @srrstats {G5.6a} Parameter recovery tests are expected to be within a defined tolerance rather than exact values.
#' @srrstats {G5.7} Unit tests include algorithm performance checks to test that the function performs as expected as parameters change.
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
#' @param x An object of class \code{rga}, which contains the results from the RGA model.
#' @param conf_bounds Logical; include confidence bounds (default: TRUE).
#' @param legend Logical; show the legend (default: TRUE).
#' @param log Logical; use a log-log scale (default: FALSE).
#' @param legend_pos Position of the legend (default: "bottomright").
#' @param ... Additional arguments passed to \code{plot()}.
#' @family Reliability Growth Analysis
#' @return Invisibly returns \code{NULL}.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' plot(result,
#'   main = "Reliability Growth Analysis",
#'   xlab = "Cumulative Time", ylab = "Cumulative Failures"
#' )
#' @importFrom graphics lines abline legend plot
#' @export
plot.rga <- function(x,
                     conf_bounds = TRUE,
                     legend = TRUE,
                     log = FALSE,
                     legend_pos = "bottomright",
                     ...) {
  # Input validation
  if (!inherits(x, "rga")) {
    stop("'x' must be an object of class 'rga'.")
  }
  if (!is.logical(conf_bounds) || length(conf_bounds) != 1) {
    stop("'conf_bounds' must be a single logical value.")
  }
  if (!is.logical(legend) || length(legend) != 1) {
    stop("'legend' must be a single logical value.")
  }
  if (!is.logical(log) || length(log) != 1) {
    stop("'log' must be a single logical value.")
  }
  if (!is.character(legend_pos) || length(legend_pos) != 1) {
    stop("'legend_pos' must be a single character string.")
  }

  if (!all(c("log_times", "log_cum_failures") %in% names(x$model$model))) {
    stop("The 'rga' object appears malformed or missing model data.")
  }

  times <- exp(x$model$model$log_times)
  cum_failures <- exp(x$model$model$log_cum_failures)

  # Base plot
  plot_args <- list(
    x = times,
    y = cum_failures,
    pch = 16,
    ...
  )
  if (log) plot_args$log <- "xy"
  do.call(graphics::plot, plot_args)

  # Fitted line
  graphics::lines(times, x$fitted_values, ...)

  # Confidence bounds
  if (conf_bounds) {
    graphics::lines(times, x$lower_bounds, lty = 2, ...)
    graphics::lines(times, x$upper_bounds, lty = 2, ...)
  }

  # Breakpoints
  if (!is.null(x$breakpoints)) {
    graphics::abline(v = exp(x$breakpoints), lty = 3)
  }

  # Legend
  if (legend) {
    legend_items <- list(
      labels = c("Observed", "Fitted Line"),
      cols = c("black", "black"),
      pch = c(16, NA),
      lty = c(NA, 1)
    )

    if (conf_bounds) {
      legend_items$labels <- c(legend_items$labels, "Confidence Bounds")
      legend_items$cols <- c(legend_items$cols, "black")
      legend_items$pch <- c(legend_items$pch, NA)
      legend_items$lty <- c(legend_items$lty, 2)
    }

    if (!is.null(x$breakpoints)) {
      legend_items$labels <- c(legend_items$labels, "Change Points")
      legend_items$cols <- c(legend_items$cols, "black")
      legend_items$pch <- c(legend_items$pch, NA)
      legend_items$lty <- c(legend_items$lty, 3)
    }

    graphics::legend(
      legend_pos,
      legend = legend_items$labels,
      col = legend_items$cols,
      pch = legend_items$pch,
      lty = legend_items$lty,
      bty = "n"
    )
  }

  invisible(NULL)
}
