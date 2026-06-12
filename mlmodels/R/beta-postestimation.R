## PREDICT =====================================================================
# Remember that the parameter documentation is done at the generic predict for
# mlmodel class.

#' @details
#' ### ml_beta prediction types
#'
#' The `type` argument controls what quantity is returned.
#'
#' | Type          | Description                              | Notes |
#' |---------------|------------------------------------------|-------|
#' | `"link"`      | Linear mean predictor ( xb )             | logit-mean |
#' | `"response"`  | Expected proportion (outcome)            | Default |
#' | `"mean"`      | Alias for `"response"`                   | - |
#' | `"fitted"`    | Alias for `"response"`                   | - |
#' | `"odds"`      | Odds ratio                               | exp(xb) |
#' | `"zd"`        | Linear precision predictor               | log-phi |
#' | `"phi"`       | Dispersion parameter                     | - |
#' | `"shape1"`    | Shape parameter of the beta distribution | mu * phi |
#' | `"shape2"`    | Shape parameter of the beta distribution | (1 - mu) * phi |
#' | `"mode"`      | Mode prediction (See below)              | (shape1 - 1) / (shape1 + shape2 - 2) |
#' | `"variance"`  | Variance of the outcome variable         | mu * (1 - mu) / (1 + phi) |
#' | `"var"`       | Alias for `"variance"`                   | - |
#' | `"sigma"`     | Standard deviation of outcome variable   | sqrt(`"variance"`) |
#' | `"sd"`        | Alias for `"sigma"`                      | - |
#'
#' When `se.fit = TRUE`, standard errors are computed using the delta method
#' for all supported types.
#' 
#' **Mode Indeterminations**
#' 
#' The mode is only defined if \code{shape1 > 1} **and** \code{shape2 > 1} **and** \code{shape1 + shape2 != 2}. If these conditions are not met the prediction and standard
#' error will be `NA`.
#'
#' @rdname predict.mlmodel
#' @method predict ml_beta
#' @export
predict.ml_beta <- function(object,
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
  if (!inherits(object, "ml_beta"))
    cli::cli_abort("`object` must be of class 'ml_gamma'.")
  
  type <-  rlang::arg_match(type, c("response", "fitted", "mean", "odds", "link", "zd", "phi", "shape1", "shape2", "mode", "variance", "var", "sd", "sigma"))
  
  # -- Prepare predictors (using hardhat) --------------------------------------
  is_heteroskedastic <- !is.null(object$model$scale_formula)
  predictors <- .prepare_prediction_data(object, newdata = newdata)
  X <- predictors$X
  Z <- predictors$Z
  
  # -- Extract coefficients and compute linear predictors ----------------------
  coefs <- coef(object)
  k_mean <- ncol(X)
  beta <- coefs[1:k_mean]
  delta <- coefs[(k_mean + 1):length(coefs)]
  
  xb <- as.vector(X %*% beta)
  zd <- as.vector(Z %*% delta)
  mu <- plogis(xb)
  odds <- exp(xb)
  phi <- exp(zd)
  var <- dlogis(xb) / (1 + phi)
  shape1 <- mu * phi
  shape2 <- plogis(xb, lower.tail = FALSE) * phi
  mode <- (shape1 - 1) / (shape1 + shape2 - 2)
  mode[shape1 + shape2 == 2] <- NA_real_
  mode[shape1 <= 1 | shape2 <= 1] <- NA_real_
  
  out <- switch(type,
                "link"     = xb,
                "mean"     = ,
                "fitted"   = ,
                "response" = mu,
                "odds"     = odds,
                "phi"      = phi,
                "zd"       = zd,
                "shape1"   = shape1,
                "shape2"   = shape2,
                "mode"     = mode,
                "var"      = ,
                "variance" = var,
                "sd"       = ,
                "sigma"    = sqrt(var),
                cli::cli_abort("Unknown prediction type '{type}'.", 
                               call = NULL))
  
  # -- Align in-sample predictions to original data length ---------------------
  if (is.null(newdata)) {
    out <- .predict_align_estimates(object, out)
  }
  
  if (!se.fit)
  {
    res <- list(
      fit = out,
      se.fit = NULL
    )
    class(res) <- c("predict.ml_gamma", "predict.mlmodel")
    return(res)
  }
  
  g_null_delta <- matrix(0, nrow = nrow(X), ncol = ncol(Z))
  g_null_beta <- matrix(0, nrow = nrow(X), ncol = ncol(X))
  g_mu_b <- dlogis(xb) * X
  g_odds_b <- odds * X
  g_phi_d <- phi * Z
  g_shape1_b <- dlogis(xb) * phi * X
  g_shape1_d <- mu * phi * Z
  g_shape2_b <- - g_shape1_b
  g_shape2_d <- shape2 * Z
  s_mode_b <- dlogis(xb) * phi / (phi - 2)
  s_mode_d <- phi * (1 - 2 * mu) / ((phi - 2)^2)
  s_mode_b[shape1 + shape2 == 2] <- s_mode_d[shape1 + shape2 == 2] <- NA_real_
  s_mode_b[shape1 <= 1 | shape2 <= 1] <- s_mode_d[shape1 <= 1 | shape2 <= 1] <- NA_real_
  g_mode_b <- s_mode_b * X
  g_mode_d <- s_mode_d * Z
  g_var_b <- var * (1 - 2 * mu) * X
  g_var_d <- - var * phi / (1 + phi) * Z
  g_sig_b <- 0.5 * var^(-0.5) * g_var_b
  g_sig_d <- 0.5 * var^(-0.5) * g_var_d
  
  g <- switch(type,
              "link"     = cbind(X, g_null_delta),
              "mean"     = ,
              "fitted"   = ,
              "response" = cbind(g_mu_b, g_null_delta),
              "odds"     = cbind(g_odds_b, g_null_delta),
              "phi"      = cbind(g_null_beta, g_phi_d),
              "zd"       = cbind(g_null_beta, Z),
              "shape1"   = cbind(g_shape1_b, g_shape1_d),
              "shape2"   = cbind(g_shape2_b, g_shape2_d),
              "mode"     = cbind(g_mode_b, g_mode_d),
              "var"      = ,
              "variance" = cbind(g_var_b, g_var_d),
              "sd"       = ,
              "sigma"    = cbind(g_sig_b, g_sig_d),
              cli::cli_abort("Unknown prediction type '{parsed_type$base_type}'.", 
                             call = NULL))
  
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
  } else {
    # -- Delta-method standard errors ------------------------------------------
    se_fit <- sqrt(rowSums(g * (g %*% full_vcov)))
  }
  
  # Align in-sample SEs
  if (is.null(newdata)) {
    se_fit <- .predict_align_estimates(object, se_fit)
  }
  
  res <- list(
    fit = out,
    se.fit = se_fit
  )
  class(res) <- c("predict.ml_beta", "predict.mlmodel")
  return(res)
}

## PRINT SUMMARY ===============================================================
#' @export
print.summary.ml_beta <- function(x, digits = max(4L, getOption("digits") - 4L), ...)
{
  if (!inherits(x, "summary.ml_beta"))
    cli::cli_abort("`x` needs to be a `summary.ml_beta` object.")
  
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
  k2 <- sum(grepl("^scale::", rownames(x$coefficients)))
  
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
  cat("Scale (log(phi)):")
  cat("  ", captured[(k1+2):(k1+k2+1)],
      sep = "\n  ")
  # It seems as if there is an empty line between the coefficients and the legend
  # so we need to add one more line and start at k1+k2+3, to avoid that empty one.
  cat("---------------------------------------",
      captured[(k1+k2+3):length(captured)],
      sep = "\n")
  
  options(scipen = old_pen)
  
  if (x$converged) {
    cat("---\n")
    cat("Number of observations:", x$nobs, 
        " Deg. of freedom: ", x$df.residual, "\n", sep = "")
    cat("Pseudo R-squared - Cor.Sq.: ",
        format(x$r.squared$cor, digits = digits),
        "\n",
        sep = "")
    cat("AIC:", format(x$AIC, nsmall = 2, digits = digits + 1),
        " BIC:", format(x$BIC, nsmall = 2, digits = digits + 1), "\n")
    if(x$is_heteroskedastic)
    {
      cat("\nDistribution of Predicted Precision (phi):",
          "---------------------------------------",
          sep = "\n")
      print(x$phihat, digits = 2)
      cat("\n")
    }
    else
      cat(sprintf("Precision Param.: %.2f\n", x$phihat[1]))
  } else {
    cat("\nGoodness-of-fit statistics not available (model did not converge).\n")
  }
  
  invisible(x)
}

## SUMMARY =====================================================================
#' @rdname summary.mlmodel
#' @export
summary.ml_beta <- function(object,
                            correlation = FALSE,
                            vcov = NULL,           # User-supplied variance matrix
                            vcov.type = "oim",
                            cl_var = NULL,
                            repetitions = 999,
                            seed = NULL,
                            progress = FALSE,
                            ...)
{
  if (!inherits(object, "ml_beta"))
    cli::cli_abort("`object` must be a model of class 'ml_beta'.")
  
  converged <- object$code %in% c(0, 1, 2, 8)
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
  
  n <- object$model$n_used
  k_total <- length(coef(object))
  k_scale <- ncol(object$model$scale$predictors)
  is_heteroskedastic <- !is.null(object$model$scale_formula)
  k_mean <- k_total - k_scale
  
  # Start building the summary object
  s <- list()
  
  # Store the response's variable name
  s$response_name <- object$model$response_name
  
  # Call helper for variance type general description.
  s$var_description <- .vcov_description(vcov_mat)
  
  # Basic information
  s$model_type <- object$model$description
  s$logLik <- as.numeric(object$maximum %||% NA_real_)
  s$call           <- object$call                    # <- Now using root-level call
  s$formula        <- object$model$formula
  s$scale_formula  <- object$model$scale_formula
  s$nobs           <- n
  s$df.residual    <- n - k_mean
  s$converged      <- converged
  s$is_heteroskedastic <- is_heteroskedastic
  
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
    y <- object$model$value$outcomes[[1]]
    yhat <- object$model$fitted.values
    s$phihat <- summary(object$model$phihat)
    
    ll <- s$logLik
    s$AIC            <- -2 * ll + 2 * k_total
    s$BIC            <- -2 * ll + log(n) * k_total
    
    y_bar <- mean(y)
    
    s$r.squared <- list(
      cor = cor(y, yhat)^2
    )
    
    if(usable_vcov)
    {
      # Joint significance tests (reuse vcov_mat)
      idx_mean <- if (object$model$value$blueprint$intercept) 2:k_mean else 1:k_mean
      
      if (is_heteroskedastic) {
        idx_scale <- if (object$model$scale$blueprint$intercept) (k_mean + 2):k_total
        else (k_mean + 1):k_total
        
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
  
  class(s) <- c("summary.ml_beta", "summary.mlmodel", "summary")
  s
}