#' @title Print Method for Generalized Kumaraswamy Regression Models
#'
#' @description
#' Print method for objects of class \code{"gkwreg"}. Provides a concise summary
#' of the fitted model following the style of \code{\link[stats]{print.lm}}.
#'
#' @param x An object of class \code{"gkwreg"}, typically obtained from
#'   \code{\link{gkwreg}}.
#' @param digits Minimum number of significant digits to print. Default is
#'   \code{max(3, getOption("digits") - 3)}.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @details
#' The print method provides a concise overview of the fitted model, showing:
#' the call, deviance residuals summary, coefficient estimates, link functions,
#' and basic fit statistics. For more detailed output including standard errors
#' and significance tests, use \code{\link{summary.gkwreg}}.
#'
#' @return The object \code{x}, invisibly.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{summary.gkwreg}}
#'
#' @examples
#' \donttest{
#' data(GasolineYield)
#' fit <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#' print(fit)
#'
#' # With more digits
#' print(fit, digits = 5)
#' }
#'
#' @method print gkwreg
#' @export
print.gkwreg <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  # Validate input
  if (!inherits(x, "gkwreg")) {
    stop("'x' must be of class 'gkwreg'", call. = FALSE)
  }

  # Header and Call
  cat("\nCall:  ")
  print(x$call)
  cat("\n")

  # Deviance Residuals
  if (!is.null(x$residuals) && length(x$residuals) > 0) {
    resids <- x$residuals
    if (length(resids) > 5) {
      # Use quantiles like glm
      rq <- structure(quantile(resids, na.rm = TRUE), names = c("Min", "1Q", "Median", "3Q", "Max"))
      cat("Deviance Residuals: \n")
      print(rq, digits = digits)
    } else {
      # Print all if few residuals
      cat("Deviance Residuals:\n")
      print(resids, digits = digits)
    }
    cat("\n")
  }

  # Coefficients
  if (!is.null(x$coefficients)) {
    cat("Coefficients:\n")
    print(x$coefficients, digits = digits)
    cat("\n")
  }

  # Link Functions
  if (!is.null(x$link)) {
    links <- x$link
    link_text <- paste(names(links), "=", unlist(links), collapse = ", ")
    cat("Link functions:  ", link_text, "\n\n", sep = "")
  }

  # Degrees of Freedom
  nobs <- x$nobs
  npar <- x$npar
  df_residual <- x$df.residual

  if (!is.null(nobs) && !is.null(df_residual)) {
    cat(sprintf(
      "Degrees of Freedom: %d Total (i.e. Null);  %d Residual\n",
      nobs - 1, df_residual
    ))
  }

  # Deviance and Information Criteria
  if (!is.null(x$deviance)) {
    # Null deviance (if available)
    if (!is.null(x$null.deviance)) {
      cat(sprintf("Null Deviance:\t    %s \n", format(x$null.deviance, digits = digits)))
    }

    # Residual deviance
    cat(sprintf("Residual Deviance: %s", format(x$deviance, digits = digits)))

    # AIC on same line
    if (!is.null(x$aic)) {
      cat(sprintf("\tAIC: %s", format(x$aic, digits = digits)))
    }
    cat("\n")
  }

  # Additional Fit Statistics (GKw specific)
  has_stats <- FALSE
  stat_line <- character(0)

  if (!is.null(x$loglik)) {
    stat_line <- c(stat_line, sprintf("Log-Lik: %s", format(x$loglik, digits = digits)))
    has_stats <- TRUE
  }

  if (!is.null(x$bic)) {
    stat_line <- c(stat_line, sprintf("BIC: %s", format(x$bic, digits = digits)))
    has_stats <- TRUE
  }

  if (has_stats) {
    cat(paste(stat_line, collapse = ",  "), "\n")
  }

  # Goodness of Fit Measures (GKw specific)
  has_gof <- FALSE
  gof_line <- character(0)

  if (!is.null(x$efron_r2)) {
    gof_line <- c(gof_line, sprintf("R-squared: %s", format(x$efron_r2, digits = digits)))
    has_gof <- TRUE
  }

  if (!is.null(x$rmse)) {
    gof_line <- c(gof_line, sprintf("RMSE: %s", format(x$rmse, digits = digits)))
    has_gof <- TRUE
  }

  if (has_gof) {
    cat(paste(gof_line, collapse = ",  "), "\n")
  }

  # Convergence Information
  cat("\n")

  # Convergence status
  converged <- if (is.null(x$convergence)) TRUE else !x$convergence

  if (!converged) {
    cat("Warning: Algorithm did not converge\n")
  }

  # Iterations
  if (!is.null(x$iterations)) {
    cat(sprintf("Number of Fisher Scoring iterations: %d\n", x$iterations))
  }

  # Convergence message if failed
  if (!converged && !is.null(x$message)) {
    cat(sprintf("Message: %s\n", x$message))
  }

  cat("\n")

  invisible(x)
}


#' @title Extract Coefficients from a Fitted GKw Regression Model
#'
#' @description
#' Extracts the estimated regression coefficients from a fitted Generalized
#' Kumaraswamy (GKw) regression model object of class \code{"gkwreg"}. This is
#' an S3 method for the generic \code{\link[stats]{coef}} function.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a
#'   call to \code{\link{gkwreg}}.
#' @param ... Additional arguments, currently ignored by this method.
#'
#' @details
#' This function provides the standard way to access the estimated regression
#' coefficients from a model fitted with \code{\link{gkwreg}}. It simply
#' extracts the \code{coefficients} component from the fitted model object.
#' The function \code{\link[stats]{coefficients}} is an alias for this function.
#'
#' @return A named numeric vector containing the estimated regression coefficients
#'   for all modeled parameters. The names indicate the parameter (e.g., `alpha`,
#'   `beta`) and the corresponding predictor variable (e.g., `(Intercept)`, `x1`).
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{summary.gkwreg}},
#'   \code{\link[stats]{coef}}, \code{\link[stats]{confint}}
#'
#' @keywords coefficients methods regression
#'
#' @export
coef.gkwreg <- function(object, ...) {
  object$coefficients
}


#' @title Summary Method for Generalized Kumaraswamy Regression Models
#'
#' @description
#' Computes and returns a detailed statistical summary for a fitted Generalized
#' Kumaraswamy (GKw) regression model object of class \code{"gkwreg"}.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a
#'   call to \code{\link{gkwreg}}.
#' @param conf.level Numeric. The desired confidence level for constructing
#'   confidence intervals for the regression coefficients. Default is 0.95.
#' @param ... Additional arguments, currently ignored by this method.
#'
#' @details
#' This method provides a comprehensive summary of the fitted \code{gkwreg} model.
#' It calculates z-values and p-values for the regression coefficients based on
#' the estimated standard errors (if available) and computes confidence intervals
#' at the specified \code{conf.level}. The summary includes:
#' \itemize{
#'   \item The model call.
#'   \item The distribution family used.
#'   \item A table of coefficients including estimates, standard errors, z-values,
#'     and p-values. Note: Significance stars are typically added by the
#'     corresponding \code{print.summary.gkwreg} method.
#'   \item Confidence intervals for the coefficients.
#'   \item Link functions used for each parameter.
#'   \item Mean values of the fitted distribution parameters (\eqn{\alpha, \beta, \gamma, \delta, \lambda}).
#'   \item A five-number summary (Min, Q1, Median, Q3, Max) plus the mean of the
#'     response residuals.
#'   \item Key model fit statistics (Log-likelihood, AIC, BIC, RMSE, Efron's R^2).
#'   \item Information about model convergence and optimizer iterations.
#' }
#' If standard errors were not computed (e.g., \code{hessian = FALSE} in the
#' original \code{gkwreg} call), the coefficient table will only contain estimates,
#' and confidence intervals will not be available.
#'
#' @return An object of class \code{"summary.gkwreg"}, which is a list containing
#'   the following components:
#' \item{call}{The original function call that created the \code{object}.}
#' \item{family}{Character string specifying the distribution family.}
#' \item{coefficients}{A data frame (matrix) containing the coefficient estimates,
#'   standard errors, z-values, and p-values.}
#' \item{conf.int}{A matrix containing the lower and upper bounds of the confidence
#'   intervals for the coefficients (if standard errors are available).}
#' \item{link}{A list of character strings specifying the link functions used.}
#' \item{fitted_parameters}{A list containing the mean values of the estimated
#'   distribution parameters.}
#' \item{residuals}{A named numeric vector containing summary statistics for the
#'   response residuals.}
#' \item{nobs}{Number of observations used in the fit.}
#' \item{npar}{Total number of estimated regression coefficients.}
#' \item{df.residual}{Residual degrees of freedom.}
#' \item{loglik}{The maximized log-likelihood value.}
#' \item{aic}{Akaike Information Criterion.}
#' \item{bic}{Bayesian Information Criterion.}
#' \item{rmse}{Root Mean Squared Error of the residuals.}
#' \item{efron_r2}{Efron's pseudo-R-squared value.}
#' \item{mean_absolute_error}{Mean Absolute Error of the residuals.}
#' \item{convergence}{Convergence code from the optimizer.}
#' \item{iterations}{Number of iterations reported by the optimizer.}
#' \item{conf.level}{The confidence level used for calculating intervals.}
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{print.summary.gkwreg}},
#'   \code{\link[stats]{coef}}, \code{\link[stats]{confint}}
#'
#' @keywords summary models regression
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 100
#' x1 <- runif(n, -2, 2)
#' x2 <- rnorm(n)
#' alpha_coef <- c(0.8, 0.3, -0.2)
#' beta_coef <- c(1.2, -0.4, 0.1)
#' eta_alpha <- alpha_coef[1] + alpha_coef[2] * x1 + alpha_coef[3] * x2
#' eta_beta <- beta_coef[1] + beta_coef[2] * x1 + beta_coef[3] * x2
#' alpha_true <- exp(eta_alpha)
#' beta_true <- exp(eta_beta)
#' # Use stats::rbeta as a placeholder if rkw is unavailable
#' y <- stats::rbeta(n, shape1 = alpha_true, shape2 = beta_true)
#' y <- pmax(pmin(y, 1 - 1e-7), 1e-7)
#' df <- data.frame(y = y, x1 = x1, x2 = x2)
#'
#' # Fit a Kumaraswamy regression model
#' kw_reg <- gkwreg(y ~ x1 + x2 | x1 + x2, data = df, family = "kw")
#'
#' # Generate detailed summary using the summary method
#' summary_kw <- summary(kw_reg)
#'
#' # Print the summary object (uses print.summary.gkwreg)
#' print(summary_kw)
#'
#' # Extract coefficient table directly from the summary object
#' coef_table <- coef(summary_kw) # Equivalent to summary_kw$coefficients
#' print(coef_table)
#' }
#'
#' @importFrom stats pnorm qchisq
#'
#' @export
summary.gkwreg <- function(object, conf.level = 0.95, ...) {
  # Calculate z-values and p-values
  coef_est <- object$coefficients
  se <- object$se

  if (is.null(se)) {
    coef_table <- data.frame(
      Estimate = coef_est,
      row.names = names(coef_est)
    )
  } else {
    z_values <- coef_est / se
    p_values <- 2 * pnorm(-abs(z_values))

    coef_table <- data.frame(
      Estimate = coef_est,
      `Std. Error` = se,
      `z value` = z_values,
      `Pr(>|z|)` = p_values,
      row.names = names(coef_est), check.names = FALSE
    )
  }

  # Calculate confidence intervals
  alpha <- 1 - conf.level
  z_value <- stats::qnorm(1 - alpha / 2)

  if (!is.null(se)) {
    ci_lower <- coef_est - z_value * se
    ci_upper <- coef_est + z_value * se
    conf_int <- cbind(ci_lower, ci_upper)
    colnames(conf_int) <- c(
      paste0(format(100 * alpha / 2, digits = 1), "%"),
      paste0(format(100 * (1 - alpha / 2), digits = 1), "%")
    )
    rownames(conf_int) <- names(coef_est)
  } else {
    conf_int <- NULL
  }

  # Summarize residuals
  if (!is.null(object$residuals)) {
    res_summary <- c(
      Min = min(object$residuals, na.rm = TRUE),
      Q1 = stats::quantile(object$residuals, 0.25, na.rm = TRUE),
      Median = stats::median(object$residuals, na.rm = TRUE),
      Mean = mean(object$residuals, na.rm = TRUE),
      Q3 = stats::quantile(object$residuals, 0.75, na.rm = TRUE),
      Max = max(object$residuals, na.rm = TRUE)
    )
  } else {
    res_summary <- NULL
  }

  # Create and return summary object
  result <- list(
    call = object$call,
    family = object$family,
    coefficients = coef_table,
    conf.int = conf_int,
    link = object$link,
    fitted_parameters = object$fitted_parameters,
    residuals = res_summary,
    nobs = object$nobs,
    npar = object$npar,
    df.residual = object$df.residual,
    loglik = object$loglik,
    aic = object$aic,
    bic = object$bic,
    rmse = object$rmse,
    efron_r2 = object$efron_r2,
    mean_absolute_error = object$mean_absolute_error,
    convergence = object$convergence,
    iterations = object$iterations,
    conf.level = conf.level
  )

  class(result) <- "summary.gkwreg"
  return(result)
}


#' @title Print Method for Generalized Kumaraswamy Regression Summaries
#'
#' @description
#' Formats and prints the summary output of a fitted Generalized Kumaraswamy
#' (GKw) regression model (objects of class \code{"summary.gkwreg"}).
#'
#' @param x An object of class \code{"summary.gkwreg"}, typically the result of
#'   a call to \code{\link{summary.gkwreg}}.
#' @param digits Integer, controlling the number of significant digits to print.
#'   Defaults to \code{max(3, getOption("digits") - 3)}.
#' @param signif.stars Logical. If \code{TRUE}, significance stars are printed
#'   next to the p-values in the coefficient table. Defaults to the value of
#'   \code{getOption("show.signif.stars")}.
#' @param ... Additional arguments, currently ignored by this method.
#'
#' @details
#' This is the print method for objects created by \code{\link{summary.gkwreg}}.
#' It formats the summary information for display in the console. It is typically
#' invoked automatically when \code{print()} is called on a \code{summary.gkwreg}
#' object, or simply by typing the name of the summary object in an interactive R session.
#'
#' The output includes:
#' \itemize{
#'   \item Model family and the original function call.
#'   \item Summary statistics for residuals.
#'   \item A coefficient table with estimates, standard errors, z-values, and
#'     p-values, optionally marked with significance stars (using
#'     \code{\link[stats]{printCoefmat}}).
#'   \item Confidence intervals for coefficients (if available).
#'   \item Link functions used for each parameter.
#'   \item Mean values of the fitted distribution parameters.
#'   \item Key model fit statistics (LogLik, AIC, BIC, RMSE, R^2, etc.).
#'   \item Convergence status and number of iterations.
#' }
#'
#' @return Invisibly returns the original input object \code{x}. This allows the
#'   output of \code{print()} to be assigned, but it primarily prints the formatted
#'   summary to the console.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{summary.gkwreg}}, \code{\link{gkwreg}},
#'   \code{\link[stats]{printCoefmat}}
#'
#' @keywords print methods internal hoss
#'
#' @export
print.summary.gkwreg <- function(x, digits = max(3, getOption("digits") - 3),
                                 signif.stars = getOption("show.signif.stars"), ...) {
  cat("\nGeneralized Kumaraswamy Regression Model Summary\n\n")

  # Display family
  cat("Family:", x$family, "\n\n")

  # Display call
  cat("Call:\n")
  print(x$call)

  # Display residuals summary
  if (!is.null(x$residuals)) {
    cat("\nResiduals:\n")
    print(round(x$residuals, digits = digits))
  }

  # Display coefficient table with significance stars
  cat("\nCoefficients:\n")
  coefs <- x$coefficients
  stats::printCoefmat(coefs,
    digits = digits, signif.stars = signif.stars,
    has.Pvalue = ncol(coefs) >= 4
  )

  # Display confidence intervals
  if (!is.null(x$conf.int)) {
    cat("\nConfidence intervals (", x$conf.level * 100, "%):\n", sep = "")
    print(round(x$conf.int, digits = digits))
  }

  # Display link functions
  cat("\nLink functions:\n")
  for (param in names(x$link)) {
    cat(param, ": ", x$link[[param]], "\n", sep = "")
  }

  # Display fitted parameter means
  cat("\nFitted parameter means:\n")
  for (param in names(x$fitted_parameters)) {
    cat(param, ": ", format(x$fitted_parameters[[param]], digits = digits), "\n", sep = "")
  }

  # Display model fit statistics
  cat("\nModel fit statistics:\n")
  cat("Number of observations:", x$nobs, "\n")
  cat("Number of parameters:", x$npar, "\n")
  cat("Residual degrees of freedom:", x$df.residual, "\n")
  cat("Log-likelihood:", format(x$loglik, digits = digits), "\n")
  cat("AIC:", format(x$aic, digits = digits), "\n")
  cat("BIC:", format(x$bic, digits = digits), "\n")

  if (!is.null(x$rmse)) {
    cat("RMSE:", format(x$rmse, digits = digits), "\n")
  }

  if (!is.null(x$efron_r2) && !is.na(x$efron_r2)) {
    cat("Efron's R2:", format(x$efron_r2, digits = digits), "\n")
  }

  if (!is.null(x$mean_absolute_error)) {
    cat("Mean Absolute Error:", format(x$mean_absolute_error, digits = digits), "\n")
  }

  # Display convergence information
  cat("\nConvergence status:", ifelse(as.numeric(x$convergence) == 1, "Successful", "Failed"), "\n")
  if (!is.null(x$iterations)) {
    cat("Iterations:", x$iterations, "\n")
  }

  cat("\n")
  invisible(x)
}


#' Extract Variance-Covariance Matrix from a Generalized Kumaraswamy Regression Model
#'
#' @description
#' This function extracts the variance-covariance matrix of the estimated parameters
#' from a fitted Generalized Kumaraswamy regression model. The variance-covariance
#' matrix is essential for statistical inference, including hypothesis testing and
#' confidence interval calculation.
#'
#' @param object An object of class \code{"gkwreg"}, typically the result of a call
#'   to \code{\link{gkwreg}}.
#' @param complete Logical indicating whether the complete variance-covariance matrix
#'   should be returned in case some coefficients were omitted from the original fit.
#'   Currently ignored for \code{gkwreg} objects.
#' @param ... Additional arguments (currently not used).
#'
#' @details
#' The variance-covariance matrix is estimated based on the observed information
#' matrix, which is derived from the second derivatives of the log-likelihood function
#' with respect to the model parameters. For \code{gkwreg} objects, this matrix is
#' typically computed using the TMB (Template Model Builder) automatic differentiation
#' framework during model fitting.
#'
#' The diagonal elements of the variance-covariance matrix correspond to the squared
#' standard errors of the parameter estimates, while the off-diagonal elements represent
#' the covariances between pairs of parameters.
#'
#' @return A square matrix with row and column names corresponding to the coefficients
#' in the model. If the variance-covariance matrix is not available (for example, if
#' the model was fitted with \code{hessian = FALSE}), the function returns \code{NULL}
#' with a warning.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{confint}}, \code{\link{summary.gkwreg}}
#'
#' @importFrom stats vcov
#' @method vcov gkwreg
#' @export
vcov.gkwreg <- function(object, complete = TRUE, ...) {
  # Check if the object is of class gkwreg
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be a fitted model object of class 'gkwreg'")
  }

  # Check if the variance-covariance matrix is available
  if (is.null(object$vcov)) {
    warning(
      "Variance-covariance matrix not found in the model object. ",
      "The model may have been fitted with hessian = FALSE. ",
      "Consider refitting with hessian = TRUE for valid statistical inference."
    )
    return(NULL)
  }

  # Return the variance-covariance matrix
  object$vcov
}

#' @title Number of Observations for GKw Regression Models
#'
#' @description
#' Extracts the number of observations from a fitted Generalized Kumaraswamy
#' regression model.
#'
#' @param object An object of class \code{"gkwreg"}, typically obtained from
#'   \code{\link{gkwreg}}.
#' @param ... Currently not used.
#'
#' @return Integer representing the number of observations used in model fitting.
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}
#'
#' @examples
#' \donttest{
#' data(GasolineYield)
#' fit <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#' nobs(fit)
#' }
#'
#' @importFrom stats nobs
#' @method nobs gkwreg
#' @export
nobs.gkwreg <- function(object, ...) {
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be of class 'gkwreg'", call. = FALSE)
  }

  n <- object$nobs
  if (is.null(n)) {
    n <- length(object$y)
  }

  return(as.integer(n))
}

#' Confidence Intervals for Generalized Kumaraswamy Regression Parameters
#'
#' Computes confidence intervals for model parameters in fitted gkwreg objects
#' using Wald (normal approximation) method based on asymptotic theory.
#'
#' @param object An object of class \code{"gkwreg"} from \code{\link{gkwreg}}.
#' @param parm A specification of which parameters are to be given confidence
#'   intervals, either a vector of numbers or a vector of names. If missing,
#'   all parameters are considered.
#' @param level The confidence level required. Default is 0.95.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' The confidence intervals are computed using the Wald method based on
#' asymptotic normality of maximum likelihood estimators:
#' \deqn{CI = \hat{\theta} \pm z_{\alpha/2} \times SE(\hat{\theta})}
#' where \eqn{z_{\alpha/2}} is the appropriate normal quantile and
#' \eqn{SE(\hat{\theta})} is the standard error from the Hessian matrix.
#'
#' The model must have been fitted with \code{hessian = TRUE} (the default)
#' in \code{\link{gkw_control}}. If standard errors are not available, an
#' error is raised.
#'
#' @return A matrix (or vector) with columns giving lower and upper confidence
#'   limits for each parameter. These will be labeled as (1-level)/2 and
#'   1 - (1-level)/2 in percent (by default 2.5 percent and 97.5 percent).
#'
#' @author Lopes, J. E.
#'
#' @seealso \code{\link{gkwreg}}, \code{\link{summary.gkwreg}},
#'   \code{\link[stats]{confint}}
#'
#' @examples
#' \donttest{
#' data(GasolineYield)
#' fit <- gkwreg(yield ~ batch + temp, data = GasolineYield, family = "kw")
#'
#' # 95 percent confidence intervals
#' confint(fit)
#'
#' # 90 percent confidence intervals
#' confint(fit, level = 0.90)
#'
#' # Specific parameters
#' confint(fit, parm = "alpha:(Intercept)")
#' confint(fit, parm = 1:3)
#' }
#'
#' @importFrom stats qnorm coef vcov
#' @method confint gkwreg
#' @export
confint.gkwreg <- function(object, parm, level = 0.95, ...) {
  if (!inherits(object, "gkwreg")) {
    stop("'object' must be of class 'gkwreg'", call. = FALSE)
  }

  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) {
    stop("'level' must be a single number between 0 and 1", call. = FALSE)
  }

  # Get coefficients
  cf <- coef(object)

  if (is.null(cf)) {
    stop("coefficients not found in model object", call. = FALSE)
  }

  # Get standard errors
  se <- object$se

  if (is.null(se)) {
    vc <- vcov(object)
    if (!is.null(vc)) {
      se <- sqrt(diag(vc))
    } else {
      stop("standard errors not available; refit model with 'hessian = TRUE'",
        call. = FALSE
      )
    }
  }

  # Check for invalid SE
  if (any(is.na(se)) || any(!is.finite(se))) {
    warning("some standard errors are NA or infinite; intervals may be unreliable",
      call. = FALSE
    )
  }

  # Select parameters
  if (missing(parm)) {
    parm <- seq_along(cf)
  } else if (is.character(parm)) {
    parm_idx <- match(parm, names(cf))
    if (any(is.na(parm_idx))) {
      missing_parms <- parm[is.na(parm_idx)]
      stop("parameter(s) not found: ", paste(missing_parms, collapse = ", "),
        call. = FALSE
      )
    }
    parm <- parm_idx
  } else if (is.numeric(parm)) {
    if (any(parm < 1) || any(parm > length(cf))) {
      stop("'parm' indices out of range", call. = FALSE)
    }
    parm <- as.integer(parm)
  } else {
    stop("'parm' must be character or numeric", call. = FALSE)
  }

  # Compute intervals
  alpha <- 1 - level
  z_crit <- qnorm(1 - alpha / 2)

  ci_lower <- cf[parm] - z_crit * se[parm]
  ci_upper <- cf[parm] + z_crit * se[parm]

  # Create matrix
  ci <- cbind(ci_lower, ci_upper)

  # Set names
  pct <- format(100 * c(alpha / 2, 1 - alpha / 2), trim = TRUE)
  colnames(ci) <- paste(pct, "%")
  rownames(ci) <- names(cf)[parm]

  return(ci)
}
