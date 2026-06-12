## PREDICT =====================================================================
#' @details
#' ### ml_probit prediction types
#' 
#' The `type` argument controls what quantity is returned. Behavior differs
#' depending on whether the model is homoskedastic or heteroskedastic.
#'
#'
#' | Type          | Homoskedastic case                  | Heteroskedastic case                     | Notes |
#' |---------------|-------------------------------------|------------------------------------------|-------|
#' | `"xb"`        | Linear predictor xb                 | Linear predictor xb                      | Linear predictor for value |
#' | `"response"`  | P(y=1 \| x)                         | P(y=1 \| x)                              | Prob. of success (default) |
#' | `"prob"`      | Alias for `"response"`              | Alias for `"response"`                   | - |
#' | `"fitted"`    | Alias for `"response"`              | Alias for `"response"`                   | - |
#' | `"prob0"`     | P(y=0 \| x)                         | P(y=0 \| x)                              | Prob. of failure |
#' | `"link"`      | Linear predictor xb                 | xb / exp(zd)                             | Probit index |
#' | `"odds"`      | Odds = prob / prob0                 | Odds = prob / prob0.                     | - |
#' | `"sigma"`     | 1 (constant)                        | Std. Deviation: exp(zd)                  | Only available if heteroskedastic |
#' | `"variance"`  | 1 (constant)                        | Variance: exp(2*zd)                      | Only available if heteroskedastic |
#' | `"zd"`        | 0 (constant)                        | Linear predictor zd                      | Linear predictor for scale |
#'
#' In binary probit models, the **overall scale** of the latent error term is 
#' not identified and is normalized to 1. In the homoskedastic case there is 
#' no scale equation, so sigma is fixed at 1. In the heteroskedastic case, 
#' the scale equation has no intercept. Therefore, the predicted `"sigma"` 
#' and `"variance"` represent **individual-level deviations** from the 
#' normalized overall scale, not the absolute standard deviation or variance.
#' 
#' The `"link"` type returns the value on the **probit scale**, which is the
#' inverse of the standard normal cumulative distribution function
#' (p = Phi^(-1)(p)). This is the linear prediction (p = xb) ih homoskedastic models,
#' and the standardized linear predictor (p = xb / sigma) in heteroskedastic models.
#'
#' When `se.fit = TRUE`, standard errors are computed using the delta method.
#' Standard errors are not available (and will return `NA`) for `"sigma"`,
#' `"variance"`, and `"zd"` in homoskedastic models.
#'
#' @rdname predict.mlmodel
#' @method predict ml_probit
#' @export
predict.ml_probit <- function(object,
                             newdata = NULL,
                             type = "response",
                             se.fit = FALSE,
                             vcov = NULL,
                             vcov.type = "oim",
                             cl_var = NULL,
                             repetitions = 999,
                             seed = NULL,
                             progress = FALSE,
                             ...)
{
  if (!inherits(object, "ml_probit"))
    cli::cli_abort("`object` must be of class 'ml_probit'.")
  
  # Match type argument
  type <- rlang::arg_match(type, c("response", "prob", "prob0", "link", "odds", "fitted",
                                   "sigma", "variance", "zd", "xb"))
  
  is_heteroskedastic <- !is.null(object$model$scale)
  predictors <- .prepare_prediction_data(object, newdata = newdata)
  X <- predictors$X
  Z <- predictors$Z
  
  n_obs <- nrow(X)
  
  # Extract coefficients
  cfs <- coef(object)
  beta <- cfs[1:ncol(X)]
  delta <- if (is_heteroskedastic) cfs[(ncol(X)+1):length(cfs)] else NULL
  
  # -- Compute the requested type ----------------------------------------------
  xb <- as.vector(X %*% beta)
  
  if (is_heteroskedastic) {
    zd    <- as.vector(Z %*% delta)
    sigma <- exp(zd)
  } else {
    zd    <- rep(0, n_obs)
    sigma <- rep(1, n_obs)
  }
  
  p1 <- pnorm(xb / sigma)
  p0 <- pnorm(- xb /sigma)
  den <- dnorm(xb / sigma)
  
  # Compute the requested prediction type
  out <- switch(type,
                "xb"       = xb,
                "link"     = xb / sigma,
                "odds"     = p1 / p0,
                "response" = ,
                "prob"     = ,
                "fitted"   = p1,
                "prob0"    = p0,
                "sigma"    = sigma,
                "variance" = sigma^2,
                "zd"       = zd,
                cli::cli_abort("Unknown prediction type '{type}'.", call = NULL)
  )
  
  # Align in-sample predictions if needed
  if (is.null(newdata)) {
    out <- .predict_align_estimates(object, out)
  }
  
  if (!se.fit)
  {
    res <- list(
      fit = out,
      se.fit = NULL
    )
    class(res) <- c("predict.ml_probit", "predict.mlmodel")
    return(res)
  }
  # -- Standard errors (delta method) ------------------------------------------
  se_fit <- NULL
  # Common dimensions
  n_obs   <- length(xb)
  n_beta  <- length(beta)
  # No n_delta because in homoskedastic case it doesn't exist.
  if (!is_heteroskedastic) {
    if(type %in% c("sigma", "variance", "zd"))
    {
      cli::cli_warn(
        "Standard errors are not available for prediction type '{type}' in homoskedastic models.",
        call = NULL
      )
      se_fit <- rep(NA_real_, n_obs)
    }
    else
    {
      # Homoskedastic: no delta. Only X dimension to the matrix.
      g_link <- X
      g_odds <- as.vector(den / p0^2) * X
      g_response <- den * X
      g <- switch(type,
                  "xb"       = ,
                  "link"     = g_link,
                  "odds"     = g_odds,
                  "response" = ,
                  "prob"     = ,
                  "fitted"   = g_response,
                  "prob0"    = - g_response,
                  matrix(0, n_obs, n_beta)
      )
    }
  } else {
    # Heteroskedastic
    n_delta <- length(delta)
    # Now the functions with respect to both set of parameters
    g_xb_beta <- X
    g_xb_delta <- matrix(0, nrow = n_obs, ncol = n_delta)
    g_link_beta <- X / sigma
    g_link_delta <- as.vector(- xb / sigma) * Z
    g_odds_beta <- as.vector(den / p0^2) * X / sigma
    g_odds_delta <- as.vector(- den / p0^2 * xb /sigma) * Z
    g_response_beta <- as.vector(den / sigma) * X
    g_response_delta <- as.vector(- den * xb / sigma) * Z
    g_sigma_beta <- matrix(0, nrow = n_obs, ncol = n_beta)
    g_sigma_delta <- sigma * Z
    g_variance_beta <- matrix(0, nrow = n_obs, ncol = n_beta)
    g_variance_delta <- 2 * sigma^2 * Z
    g_zd_beta <- matrix(0, nrow = n_obs, ncol = n_beta)
    g_zd_delta <- Z
    
    g <- switch(type,
                "xb"       = cbind(g_xb_beta,
                                   g_xb_delta),
                "link"     = cbind(g_link_beta,
                                   g_link_delta),
                "odds"     = cbind(g_odds_beta,
                                   g_odds_delta),
                "response" = ,
                "prob"     = ,
                "fitted"   = cbind(g_response_beta,
                                   g_response_delta),
                "prob0"    = - cbind(g_response_beta,
                                     g_response_delta),
                "sigma"    = cbind(g_sigma_beta,
                                   g_sigma_delta),
                "variance" = cbind(g_variance_beta,
                                   g_variance_delta),
                "zd"       = cbind(g_zd_beta,
                                   g_zd_delta),
                matrix(0, n_obs, n_beta + n_delta)
    )
  }
  
  full_vcov <- .process_vcov(object,
                             vcov = vcov,
                             vcov.type = vcov.type,
                             cl_var = cl_var,
                             repetitions = repetitions,
                             seed = seed,
                             progress = progress)
  
  # -- Check for unusable variance matrix --------------------------------------
  if (any(!is.finite(full_vcov)) || any(is.na(full_vcov))) {
    cli::cli_warn(
      c("Variance matrix is unusable (contains NAs or non-finite values).",
        "i" = "This usually happens with bootstrap when constraints are present.",
        "i" = "Standard errors will be returned as NA.")
    )
    se_fit <- rep(NA_real_, length(out))
  }
  
  # Check fractional response inference for oim or opg
  .fractional_response_inference_alert(object, full_vcov)
  
  # We may have se_fit being NA from this final check and the one we did in the
  # homoskedastic case for sigma, zd, and variance types.
  if(is.null(se_fit))
    se_fit <- sqrt(rowSums(g * (g %*% full_vcov)))
  
  # Align in-sample SEs
  if (is.null(newdata)) {
    se_fit <- .predict_align_estimates(object, se_fit)
  }
  res <- list(
    fit = out,
    se.fit = se_fit
  )
  class(res) <- c("predict.ml_probit", "predict.mlmodel")
  return(res)
}

## PRINT SUMMARY ===============================================================
#' @export
print.summary.ml_probit <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
  if (!inherits(x, "summary.ml_probit"))
    cli::cli_abort("`x` needs to be a `summary.ml_probit` object.")
  
  cat("\nMaximum Likelihood Model\n")
  cat(" Type:", x$model_type, "\n")
  cat("---------------------------------------\n")
  
  if (!is.null(x$call)) {
    cat("Call:\n")
    print(x$call)
    cat("\n")
  }
  
  if (!x$converged) {
    cat("WARNING: Model did NOT converge!\n")
    cat("Convergence code:", x$code %||% "???", "-", x$message %||% "", "\n\n")
  } else {
    # Log-Likelihood + Joint tests (only when converged)
    cat("Log-Likelihood:", format(x$logLik, nsmall = 2, digits = digits + 1), "\n\n")
    cat("Wald significance tests:\n")
    
    any_test_printed <- FALSE
    for (test in c("all", "mean", "scale")) {
      w <- x$significance[[test]]
      if (is.null(w) || isTRUE(w$singular) || !is.finite(w$pval)) {
        next   # skip silently (happens in homoskedastic case or useless variance)
      }
      any_test_printed <- TRUE
      p_str <- if (w$pval < 1e-8) "< 1e-8" else sprintf("%.4f", w$pval)
      cat(sprintf(" %s: Chisq(%d) = %.3f, Pr(>Chisq) = %s\n",
                  tools::toTitleCase(test), w$df, w$waldstat, p_str))
    }
    
    if (!any_test_printed) {
      cat(" Tests were not computable (singular or not finite variance).\n")
    }
  }
  
  cat("\nVariance type:", x$var_description)
  cat("\n---------------------------------------\n")
  
  old_pen <- getOption("scipen")
  options(scipen = .mlmodels_get_default("scipen"))
  
  # Determining the number of leading zeroes in the estimates and standard errors.
  format_coef <- .format_coef_matrix(x$coefficients, digits = digits)
  
  # Capture the whole output of printCoefmat into a vector of strings.
  captured <- capture.output(printCoefmat(format_coef,
                                          digits = digits,
                                          signif.legend = TRUE))
  
  # Get the number of coefficients in each equation.
  k1 <- sum(grepl("^value::", rownames(x$coefficients)))
  if(x$is_heteroskedastic)
    k2 <- sum(grepl("^scale::", rownames(x$coefficients)))
  else
    k2 <- 0
  
  # The first row is the header of the table, have to indent it to align it with
  # the coefficients after.
  cat("  ", captured[1],"\n")
  # Value header. Depends if we have the dependent's variable name.
  val_head <- if (!is.null(x$response_name) && 
                  nzchar(trimws(x$response_name))) {
    paste0("Value (", trimws(x$response_name), "):")
  } else {
    "Value:"
  }
  cat(val_head)
  cat("  ", captured[2:(k1+1)],
      sep = "\n  ")
  if(x$is_heteroskedastic)
  {
    cat("Scale (log(sigma)):")
    cat("  ", captured[(k1+2):(k1+k2+1)],
        sep = "\n  ")
  }
  # It seems as if there is an empty line between the coefficients and the legend
  # so we need to add one more line and start at k1+k2+3, to avoid that empty one.
  cat("---------------------------------------",
      captured[(k1+k2+3):length(captured)],
      sep = "\n")
  
  options(scipen = old_pen)
  
  if (x$converged) {
    cat("---\n")
    cat("Number of observations:", x$nobs, 
        " (Successes: ", x$n_success, ", Failures: ", x$n_failure, ")\n", sep = "")
    cat("Pseudo R-squared - Cor.Sq.: ",
        format(x$r.squared$cor, digits = digits),
        " McKelvey & Zavoina: ", format(x$r.squared$mczav, digits = digits),
        "\n",
        sep = "")
    cat("AIC:", format(x$AIC, nsmall = 2, digits = digits + 1),
        " BIC:", format(x$BIC, nsmall = 2, digits = digits + 1), "\n")
    if(x$is_heteroskedastic)
    {
      cat("\nDistribution of Std. Deviation (sigma):",
          "---------------------------------------",
          sep = "\n")
      print(x$sigma, digits = 2)
      cat("\n")
    }
  } else {
    cat("\nGoodness-of-fit statistics not available (model did not converge).\n")
  }
  
  invisible(x)
}

## SUMMARY =====================================================================
#' @rdname summary.mlmodel
#' @export
summary.ml_probit <- function(object,
                              correlation = FALSE,
                              vcov = NULL,
                              vcov.type = "oim",
                              cl_var = NULL,
                              repetitions = 999,
                              seed = NULL,
                              progress = FALSE,
                              ...)
{
  if (!inherits(object, "ml_probit"))
    cli::cli_abort("`object` must be a model of class 'ml_probit'.")
  
  converged <- object$code %in% c(0L, 1L, 2L, 8L)
  # Get variance-covariance matrix once
  vcov_mat <- .process_vcov(object,
                            vcov = vcov,
                            vcov.type   = vcov.type,
                            cl_var      = cl_var,
                            repetitions = repetitions,
                            seed        = seed,
                            progress    = progress)
  
  # Check if the variance matrix is usable
  usable_vcov <- TRUE
  if (any(!is.finite(vcov_mat)) || any(is.na(vcov_mat))) {
    usable_vcov <- FALSE
    warn_msg <- "Variance matrix is not usable (contains NAs or non-finite values)."
  } else if (!.is_invertible(vcov_mat)) {
    usable_vcov <- FALSE
    warn_msg <- "Variance matrix is not invertible (likely singular or nearly singular)."
  }
  if(!usable_vcov)
    cli::cli_warn(
      c(warn_msg,
        "i" = "This can happen with bootstrap under constraints or in models with high collinearity.",
        "i" = "Joint significance tests and correlation matrix will be skipped.",
        "i" = "Consider using `vcov.type = 'robust'` for more stable results.")
    )
  
  # Extract observations, and number of parameters.
  # We extract out here to calculate the number of success and failures
  y <- object$model$value$outcomes[[1]]
  n <- object$model$n_used
  n1 <- sum(y)
  n0 <- n - n1
  k_total <- length(coef(object))
  k_scale <- if (!is.null(object$model$scale)) ncol(object$model$scale$predictors) else 0L
  is_heteroskedastic <- !is.null(object$model$scale)
  k_mean <- k_total - k_scale
  
  # Start building the summary object
  s <- list()
  
  s$logLik <- as.numeric(object$maximum %||% NA_real_)
  s$response_name <- object$model$response_name
  s$call          <- object$call
  s$formula       <- object$model$formula
  s$scale_formula <- object$model$scale_formula
  s$nobs          <- n
  s$n_success     <- n1
  s$n_failure     <- n0
  s$converged     <- converged
  s$is_heteroskedastic <- is_heteroskedastic
  
  # Call helper for variance type general description.
  s$var_description <- .vcov_description(vcov_mat)
  
  # Checking for fractional response estimation, and variance
  .fractional_response_inference_alert(object, vcov_mat)
  
  # Coefficient table
  se <- sqrt(diag(vcov_mat))
  
  s$coefficients <- cbind(
    Estimate   = coef(object),
    `Std. Error` = se,
    `z value`  = coef(object) / se,
    `Pr(>|z|)` = 2 * pnorm(abs(coef(object) / se), lower.tail = FALSE)
  )
  
  # Stats if converged
  if (converged) {
    # y was pulled at the beginning to calculate the number of successes and
    # failures.
    ll <- s$logLik
    s$AIC            <- -2 * ll + 2 * k_total
    s$BIC            <- -2 * ll + log(n) * k_total
    
    yhat <- object$model$fitted.values
    xb <- as.vector(as.matrix(object$model$value$predictors) %*% coef(object)[1:k_mean])
    
    sig <- object$model$sigma
    
    s$r.squared <- list(
      cor = cor(y, yhat)^2,
      mczav = var(xb / sig) / (1 + var(xb / sig))
    )
    
    s$sigma <- summary(object$model$sigma)
    
    if(usable_vcov)
    {
      # Joint significance tests (reuse vcov_mat)
      idx_mean <- if (object$model$value$blueprint$intercept) 2:k_mean else 1:k_mean
      
      # Avoid duplicate warning of fractional response inference from waldtest.
      suppressWarnings({
        if (is_heteroskedastic) {
          idx_scale <- (k_mean + 1):k_total
          
          s$significance <- list(
            all  = waldtest(object, indices = c(idx_mean, idx_scale), vcov = vcov_mat),
            mean = waldtest(object, indices = idx_mean, vcov = vcov_mat),
            scale = waldtest(object, indices = idx_scale, vcov = vcov_mat)
          )
        } else {
          s$significance <- list(
            all  = waldtest(object, indices = idx_mean, vcov = vcov_mat),
            mean = NULL,
            scale = NULL
          )
        }
      })
      
    }
    else
    {
      s$significance <- list(
        all  = NULL,
        mean = NULL,
        scale = NULL
      )
    }
    
  } else {
    s$r.squared <- s$AIC <- s$BIC <- s$sigma <- s$significance <- NULL
  }
  
  if(correlation && converged && usable_vcov)
    s$correlation <- cov2cor(vcov_mat)
  else
    s$correlation <- NULL
  
  s$model_type <- object$model$description
  class(s) <- c("summary.ml_probit", "summary.mlmodel", "summary")
  s
}