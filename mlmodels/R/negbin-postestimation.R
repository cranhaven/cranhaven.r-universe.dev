## PREDICT =====================================================================
# Remember that the parameter documentation is done at the generic predict for
# mlmodel class.

#' @details
#' ### ml_negbin prediction types
#'
#' The `type` argument controls what quantity is returned. In addition to
#' standard types, Negative Binomial models support flexible probability requests
#' using the `P(...)` syntax.
#'
#' | Type          | Description                              | Notes |
#' |---------------|------------------------------------------|-------|
#' | `"link"`      | Linear mean predictor ( xb )                  | log-mean |
#' | `"response"`  | Expected count ( \code{mu} = \code{exp(xb)} )          | Default |
#' | `"mean"`      | Alias for `"response"`                   | - |
#' | `"fitted"`    | Alias for `"response"`                   | - |
#' | `"zd"`        | Linear dispersion predictor              | log-alpha |
#' | `"alpha"`     | Dispersion parameter                     | - |
#' | `"variance"`  | Variance of the outcome variable         | - |
#' | `"var"`       | Alias for `"variance"`                   | - |
#' | `"sigma"`     | Standard deviation of outcome variable   | sqrt(`"variance"`) |
#' | `"sd"`        | Alias for `"sigma"`                      | - |
#' | `P(k)`        | P(Y = k)                                 | Exact probability, k integer >= 0 |
#' | `P(,k)`       | P(Y <= k)                                 | Cumulative (lower tail) |
#' | `P(k,)`       | P(Y >= k)                                 | Survival (upper tail) |
#' | `P(a,b)`      | P(a <= Y <= b)                             | Interval probability, a <= b, a >= 0 |
#'
#' When `se.fit = TRUE`, standard errors are computed using the delta method
#' for all supported types.
#'
#' @rdname predict.mlmodel
#' @method predict ml_negbin
#' @export
predict.ml_negbin <- function(object,
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
  if (!inherits(object, "ml_negbin"))
    cli::cli_abort("`object` must be of class 'ml_negbin'.")
  
  # Send type to the parser, and then check others.
  
  parsed_type <- .predict_types_parsing(type)
  
  if(parsed_type$base_type != "prob")
  {
    parsed_type$base_type <- rlang::arg_match(type,
                                              c("response", "fitted", "mean", "link", "zd", "alpha", "variance", "var", "sd", "sigma"))
  }
  
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
  mu <- exp(xb)
  alpha <- exp(zd)
  
  is_nb2 <- (object$model$dispersion == "NB2")
  
  var <- if(is_nb2) mu + alpha * mu^2 else mu + alpha * mu
  
  # Probability
  if (parsed_type$base_type == "prob") {
    
    # Safety checks. These have been done at the parser, but they're safety
    # checks.
    if (parsed_type$prob_type == "exact" && parsed_type$lower < 0) {
      cli::cli_abort("P(k) requires k >= 0.", call = NULL)
    }
    
    if (parsed_type$prob_type == "geq" && parsed_type$lower < 0) {
      cli::cli_abort("P(k,) requires k >= 0.", call = NULL)
    }
    
    if (parsed_type$prob_type == "leq" && parsed_type$upper < 0) {
      cli::cli_abort("P(,k) requires k >= 0.", call = NULL)
    }
    
    if (parsed_type$prob_type == "interval") {
      if (parsed_type$lower < 0) {
        cli::cli_abort("Lower bound in P(a,b) must be >= 0.", call = NULL)
      }
      if (parsed_type$upper <= parsed_type$lower) {
        cli::cli_abort("Invalid interval P({parsed_type$lower},{parsed_type$upper}). Upper must be greater than lower.", 
                       call = NULL)
      }
    }
    
    out <- switch(parsed_type$prob_type,
                  "exact"    = if(is_nb2) dnbinom(parsed_type$lower,
                                                  size = 1 / alpha,
                                                  mu = mu)
                               else dnbinom(parsed_type$lower,
                                            size = mu / alpha,
                                            prob = 1 / (1 + alpha)),
                  "leq"      = if(is_nb2) pnbinom(parsed_type$upper,
                                                  size = 1 / alpha,
                                                  mu = mu)
                               else pnbinom(parsed_type$upper,
                                            size = mu / alpha,
                                            prob = 1 / (1 + alpha)),
                  "geq"      = if(is_nb2) pnbinom(parsed_type$lower - 1,
                                                  size = 1 / alpha,
                                                  mu = mu,
                                                  lower.tail = FALSE)
                               else pnbinom(parsed_type$lower - 1,
                                            size = mu / alpha,
                                            prob = 1 / (1 + alpha),
                                            lower.tail = FALSE),
                  "interval" = if(is_nb2) pnbinom(parsed_type$upper,
                                                  size = 1 / alpha,
                                                  mu = mu) -
                                          pnbinom(parsed_type$lower - 1,
                                                  size = 1 / alpha,
                                                  mu = mu)
                               else pnbinom(parsed_type$upper,
                                            size = mu / alpha,
                                            prob = 1 / (1 + alpha)) -
                                    pnbinom(parsed_type$lower - 1,
                                            size = mu / alpha,
                                            prob = 1 / (1 + alpha)),
                  cli::cli_abort("Unknown probability type.", call = NULL)
    )
    
  } else {
    # Standard types (link, response, mean)
    out <- switch(parsed_type$base_type,
                  "link"     = xb,
                  "mean"     = ,
                  "fitted"   = ,
                  "response" = mu,
                  "alpha"    = alpha,
                  "zd"       = zd,
                  "var"      = ,
                  "variance" = var,
                  "sd"       = ,
                  "sigma"    = sqrt(var),
                  cli::cli_abort("Unknown prediction type '{parsed_type$base_type}'.", 
                                 call = NULL))
  }
  
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
    class(res) <- c("predict.ml_negbin", "predict.mlmodel")
    return(res)
  }
  
  n_obs <- length(mu)
  n_beta <- length(beta)
  if (parsed_type$base_type == "prob") {
    # should already have coefs but let's make sure
    coefs <- coef(object)
    # create dummy weights so we can pass them to the score function
    w <- rep(1, nrow(X))
    if(parsed_type$prob_type == "exact")   # P(k)
    {
       y_star <- rep(parsed_type$lower, nrow(X)) # vector with the y value to pass to the scores.
       scrs <- if(is_nb2)
         .ml_negbin_nb2_score(coefs, y_star, X, Z, w)
       else
         .ml_negbin_nb1_score(coefs, y_star, X, Z, w)
       g <- out * scrs # out holds the vector with the predictions.
    }
    else if(parsed_type$prob_type == "leq") # P(y <= b)
    {
      b <- parsed_type$upper
      # Initialize a matrix of zeros for the cumulative gradient
      g <- matrix(0, nrow = nrow(X), ncol = length(coefs))
      
      for (j in 0:b) {
        # 1. Calculate the point probability for this specific j
        p_j <- if(is_nb2) {
          dnbinom(j, size = 1 / alpha, mu = mu)
        } else {
          dnbinom(j, size = mu / alpha, prob = 1 / (1 + alpha))
        }
        
        # 2. Get the score matrix for this specific j
        y_star <- rep(j, nrow(X))
        scrs_j <- if(is_nb2) {
          .ml_negbin_nb2_score(coefs, y_star, X, Z, w)
        } else {
          .ml_negbin_nb1_score(coefs, y_star, X, Z, w)
        }
        
        # 3. Add this point's contribution to the total gradient
        g <- g + (p_j * scrs_j)
      }
    }
    else if(parsed_type$prob_type == "geq") # P(y >= k)
    {
      b_limit <- parsed_type$lower - 1
      g <- matrix(0, nrow = nrow(X), ncol = length(coefs))
      
      # Sum the gradients from 0 to k-1
      for (j in 0:b_limit) {
        p_j <- if(is_nb2) dnbinom(j, size = 1 / alpha, mu = mu)
        else       dnbinom(j, size = mu / alpha, prob = 1 / (1 + alpha))
        
        y_star <- rep(j, nrow(X))
        scrs_j <- if(is_nb2) .ml_negbin_nb2_score(coefs, y_star, X, Z, w)
        else       .ml_negbin_nb1_score(coefs, y_star, X, Z, w)
        
        g <- g + (p_j * scrs_j)
      }
      # The gradient of (1 - sum) is -sum
      g <- -g 
    }
    else if(parsed_type$prob_type == "interval")
    {
      a <- parsed_type$lower
      b <- parsed_type$upper
      g <- matrix(0, nrow = nrow(X), ncol = length(coefs))
      # Sum the gradients from 0 to k-1
      for (j in a:b) {
        p_j <- if(is_nb2) dnbinom(j, size = 1 / alpha, mu = mu)
        else       dnbinom(j, size = mu / alpha, prob = 1 / (1 + alpha))
        
        y_star <- rep(j, nrow(X))
        scrs_j <- if(is_nb2) .ml_negbin_nb2_score(coefs, y_star, X, Z, w)
        else       .ml_negbin_nb1_score(coefs, y_star, X, Z, w)
        
        g <- g + (p_j * scrs_j)
      }
    }
    else
    {
      # We never should be here because we matched the cases, in the parsing but
      # just in case
      cli::cli_abort("Unknown probability type '{type}'.",
                     call = NULL)
    }
  } else {
    g_null_delta <- matrix(0, nrow = nrow(X), ncol = ncol(Z))
    g_null_beta <- matrix(0, nrow = nrow(X), ncol = ncol(X))
    g_mu_b <- mu * X
    g_alpha_d <- alpha * Z
    g_var_b <- if(is_nb2) (mu + 2 * alpha * mu^2) * X else var * X
    g_var_d <- if(is_nb2) alpha * mu^2 * Z else alpha * mu * Z
    g_sig_b <- 0.5 * var^(-0.5) * g_var_b
    g_sig_d <- 0.5 * var^(-0.5) * g_var_d
    
    g <- switch(parsed_type$base_type,
                "link"     = cbind(X, g_null_delta),
                "mean"     = ,
                "fitted"   = ,
                "response" = cbind(g_mu_b, g_null_delta),
                "alpha"    = cbind(g_null_beta, g_alpha_d),
                "zd"       = cbind(g_null_beta, Z),
                "var"      = ,
                "variance" = cbind(g_var_b, g_var_d),
                "sd"       = ,
                "sigma"    = cbind(g_sig_b, g_sig_d),
                cli::cli_abort("Unknown prediction type '{parsed_type$base_type}'.", 
                               call = NULL))
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
  class(res) <- c("predict.ml_negbin", "predict.mlmodel")
  return(res)
}

## PRINT SUMMARY ===============================================================
#' @export
print.summary.ml_negbin <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
  if (!inherits(x, "summary.ml_negbin"))
    cli::cli_abort("`x` needs to be a `summary.ml_negbin` object.")
  
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
  cat("Scale (log(alpha)):")
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
        " McFadden: ", format(x$r.squared$mcfadden, digits = digits),
        "\n",
        sep = "")
    cat("AIC:", format(x$AIC, nsmall = 2, digits = digits + 1),
        " BIC:", format(x$BIC, nsmall = 2, digits = digits + 1), "\n")
    cat("\nCount Diagnostics:\n")
    cat("  Dispersion Ratio (Pearson):", format(x$ov, nsmall = 2, digits = digits + 1), "\n")
    cat("  Zeros - Observed:", x$zero$count, "Predicted:", round(x$zero$pred, 2), "\n")
    
    if(x$is_heteroskedastic)
    {
      cat("\nDistribution of Dispersion (alpha):",
          "---------------------------------------",
          sep = "\n")
      print(x$alpha, digits = 2)
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
summary.ml_negbin <- function(object,
                              correlation = FALSE,
                              vcov = NULL,           # User-supplied variance matrix
                              vcov.type = "oim",
                              cl_var = NULL,
                              repetitions = 999,
                              seed = NULL,
                              progress = FALSE,
                              ...)
{
  if (!inherits(object, "ml_negbin"))
    cli::cli_abort("`object` must be a model of class 'ml_negbin'.")
  
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
  s$logLik <- as.numeric(object$maximum %||% NA_real_)
  s$call           <- object$call                    # <- Now using root-level call
  s$formula        <- object$model$formula
  s$scale_formula  <- object$model$scale_formula
  s$nobs           <- n
  s$df.residual    <- n - k_total
  s$converged      <- converged
  s$is_heteroskedastic <- is_heteroskedastic
  s$dispersion     <- object$model$dispersion
  
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
    alphahat <- object$model$fitted.alpha
    
    ll <- s$logLik
    s$AIC            <- -2 * ll + 2 * k_total
    s$BIC            <- -2 * ll + log(n) * k_total
    
    vnb <- if(object$model$dispersion == "NB2") yhat + alphahat * yhat^2 else yhat + alphahat * yhat
    
    # overdispersion
    s$ov <- sum((y - yhat)^2 / vnb) / (n - k_total)
    
    # Null logLik
    y_bar <- mean(y)
    ll0 <- object$model$ll0
    
    s$r.squared <- list(
      cor = cor(y, yhat)^2,
      mcfadden = 1 - ll / ll0
    )
    
    pred_zero <- if(object$model$dispersion == "NB2")
      sum((1 + alphahat * yhat)^(-1 / alphahat))
    else
      sum((1 / (1 + alphahat))^(yhat / alphahat))
    
    s$zero <- list(
      count = sum(y == 0),
      pred = pred_zero
    )
    
    s$alpha <- summary(as.vector(alphahat))
    
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
    s$r.squared <- s$AIC <- s$BIC <- s$ov <- s$zero <- s$significance <- NULL
  }
  
  
  if(correlation && converged && usable_vcov)
    s$correlation <- cov2cor(vcov_mat)
  else
    s$correlation <- NULL
  
  s$model_type <- object$model$description
  class(s) <- c("summary.ml_negbin", "summary.mlmodel", "summary")
  s
}