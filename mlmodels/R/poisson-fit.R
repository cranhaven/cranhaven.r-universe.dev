#' Fit Poisson model by Maximum Likelihood
#'
#' @param value Formula for the conditional mean (value) equation.
#' @param weights Optional weights variable. It can be either the name of the
#'   variable in `data`, or a vector with the weights.
#' @param data Data frame.
#' @param subset Optional subset expression. Only observations for which this
#'   expression evaluates to `TRUE` are used in the estimation. This can be
#'   a logical vector or an expression (e.g. `subset = age > 30`).
#' @param noint_value Logical. Should the value equation omit the intercept?
#'   Default is `FALSE`.
#' @param constraints Optional constraints on the parameters. Can be a character
#'   vector of string constraints, a named list of string constraints, or a raw
#'   maxLik constraints list. See **Details**.
#' @param start Numeric vector of starting values for the coefficients. Required
#'   if constraints are being supplied. If supplied without constraints they
#'   will be ignored. See **Details**.
#' @param method A string with the method used for optimization. See
#'   [maxLik][maxLik::maxLik()] for options, and see **Details**.
#' @param start Numeric vector of starting values for the coefficients. Required
#'   if constraints are being supplied. If supplied without constraints they
#'   will be ignored. See **Details**.
#' @param control A list of control parameters passed to [maxLik][maxLik::maxLik].
#'   If `NULL` (default), a sensible set of options is chosen automatically
#'   depending on whether constraints are used. See [maxControl][maxLik::maxControl].
#' @param ... Additional arguments passed to [maxLik][maxLik::maxLik].
#'
#' @details
#' **Important:** Do not use the usual R syntax to remove the intercept in the
#' formula (`- 1` or `+ 0`) for the value equation. Use the dedicated
#' argument `noint_value` instead.
#'
#' Coefficient names in the fitted object use the prefixes `value::`. This is for
#' consistency with other `mlmodel` estimators that model the scale (dispersion)
#' as well.
#'
#' Either inequality or equality linear constraints are accepted, but not both.
#' A constraint cannot have a linear combination of more than two coefficients.
#'
#' **Important**: When `constraints` are supplied, `start` cannot be `NULL`.
#' You **must** provide initial values that yield a feasible log-likelihood.
#' If no constraints are used, any supplied `start` is ignored.
#'
#' When constraints are used, `ml_lm` automatically chooses the optimizer:
#' - Equality constraints => Nelder-Mead (`"NM"`)
#' - Inequality constraints => BFGS (`"BFGS"`)
#'
#' In these cases your supplied `method` argument (if any) is ignored.
#' 
#' The Poisson model assumes equidispersion (mean = variance). 
#' When the data show overdispersion (as is common), consider using 
#' [ml_negbin] instead.
#'
#' @return An object of class `ml_poisson` that extends `mlmodel.count` and `mlmodel`.
#' 
#' @seealso [ml_negbin]
#' 
#' @examples
#' 
#' # Poisson model
#' data(docvis)
#' fit_pois <- ml_poisson(docvis ~ age + educyr + totchr, 
#'                        data = docvis)
#' 
#' summary(fit_pois, vcov.type = "robust")
#' 
#' # Different predict types
#' head(predict(fit_pois, type = "response")$fit)   # Expected count
#' head(predict(fit_pois, type = "P(2,)")$fit)      # Probability of at least 2
#' head(predict(fit_pois, type = "P(3)")$fit)       # Probability of exactly 3
#' 
#' # Fitted values and residuals
#' head(fitted(fit_pois))
#' head(residuals(fit_pois))
#' head(residuals(fit_pois, type = "pearson"))
#'
#' @author Alfonso Sanchez-Penalver
#'
#' @export
ml_poisson <- function(value,
                       weights = NULL,
                       data,
                       subset = NULL,
                       noint_value = FALSE,
                       constraints = NULL,
                       start = NULL,
                       method = "NR",
                       control = NULL,
                       ...)
{
  # -- Basic input validation ------------------------------------------
  if (!rlang::is_formula(value, lhs = TRUE)) {
    cli::cli_abort("`value` must be a two-sided formula with an outcome variable on the left-hand side.",
                   call = NULL)
  }
  
  cl <- match.call()
  
  # Making sure we store the formulas in the call and not references to the
  # formulas.
  cl$value <- eval(value)
  
  # -- 0. Save original data dimensions and create keep vector ------------
  n_orig <- nrow(data)
  keep <- rep(TRUE, n_orig)          # Start with all observations kept
  
  data <- .convert_integers_to_double(data)
  
  # -- 1. Handle subset argument --------------------------------------
  # 1.1. Process subset using the helper
  sub_res <- .process_subset(rlang::enquo(subset), data)
  
  # 1.2. Update the call object for the summary
  cl$subset <- sub_res$expr
  
  # 1.3. Apply the subset to the data
  keep <- keep & sub_res$idx
  
  # -- 2. Weights handling ------------------------------------------
  w_expr <- rlang::enquo(weights)
  if (!rlang::quo_is_null(w_expr)) {
    # Try to see if it looks like a column name
    w_name <- tryCatch(rlang::as_name(w_expr), error = function(e) NULL)
    wts    <- rlang::eval_tidy(w_expr, data)
  } else {
    w_name <- NULL
    wts    <- NULL
  }
  
  # Safety: if user passed a vector, w_name should not be treated as a column
  if (!is.null(w_name) && !w_name %in% names(data)) {
    w_name <- NULL   # it was a direct vector, not a column
  }
  
  # -- 3. Identify usable observations on full data ----------------
  cols_to_check <- all.vars(value)
  if (!is.null(w_name)) {
    cols_to_check <- unique(c(cols_to_check, w_name))
  }
  
  usable_obs <- complete.cases(data[, cols_to_check, drop = FALSE])
  
  # Extra check if user passed a weights vector directly
  if (!is.null(wts) && is.null(w_name)) {
    usable_obs <- usable_obs & complete.cases(wts)
  }
  
  # Count observations dropped due to missing values *within the subset*
  nas_dropped <- sum(keep & !usable_obs)
  if (nas_dropped > 0) {
    cli::cli_alert_info("Dropped {nas_dropped} observations due to missing values.")
  }
  
  # -- 4. Modify sample = subset and complete cases ----------------
  sample <- keep & usable_obs
  
  # -- 5. Detect log transformation ------------------------------
  # We evaluate on the full data so invalid_idx has length = n_orig
  log_info <- .detect_log_transformations(list(value = value),
                                          data)
  
  # Count how many *additional* observations are invalid due to the log
  # (only among those that survived subset + NAs)
  n_invalid_log <- sum(sample & log_info$value$invalid_idx)
  
  if (log_info$value$is_log && n_invalid_log > 0) {
    cli::cli_alert_info(
      "Outcome is log-transformed. Dropped {n_invalid_log} observation(s) \\
       because they would produce invalid values (<= 0)."
    )
    
    # Final reduction of sample
    sample <- sample & !log_info$value$invalid_idx
  }
  
  # Add the actual dropped count to log_info for later use
  log_info$value$n_invalid_log <- n_invalid_log
  
  # -- 6. Create clean dataset for modeling ----------------------
  data_clean <- data[sample, , drop = FALSE]
  wts_clean <- if (!is.null(wts)) wts[sample] else rep(1, sum(sample))
  
  # Add this safety check:
  if (length(wts_clean) != sum(sample) || any(is.na(wts_clean))) {
    cli::cli_abort("Final weights vector has wrong length or contains NAs.", call = NULL)
  }
  
  model_value <- hardhat::mold(value,
                               data_clean,
                               blueprint = hardhat::default_formula_blueprint(intercept = !noint_value))
  
  molds <- list(
    value = model_value
  )
  
  y <- model_value$outcomes[[1]]
  x <- as.matrix(model_value$predictors)
  
  # -- 7. Map factor variables in relevant equations ---------
  factor_mapping <- .build_factor_mapping(molds)
  
  # -- 8. Managing control and constraints -------------------
  # Default control lists
  default_NR <- list(tol = -1,
                     reltol = 1e-12,
                     gradtol = 1e-12,
                     lambdatol = 1e-20,
                     qac = "marquardt")
  
  default_BFGS <- list(reltol = 1e-8)
  default_NM <- list(reltol = 1e-8,
                     iterlim = 1000)
  
  # Parse constraints (if any)
  if (!is.null(constraints)) {
    if (is.null(start))
      cli::cli_abort("Constrained optimization requires a vector of initial values (`start`).",
                     call = NULL)
    if (any(!is.numeric(start)))
      cli::cli_abort("Initial values must be numeric.", call = NULL)
    if (length(start) != (ncol(x)))
      cli::cli_abort("The vector of initial values has the wrong dimension. It requires {.val {ncol(x) + ncol(z)}} values.")
    
    coef_names <- paste0("value::", colnames(x))
    parsed_constraints <- .parse_constraints(constraints, coef_names)
    
    if (!is.null(parsed_constraints$maxLik$eqA))
    {
      cli::cli_alert_info("Equality constraints detected => using Nelder-Mead optimizer.")
      method <- "NM"
      if (is.null(control))
        control <- default_NM
    }
    else
    {
      method = "BFGS"
      cli::cli_alert_info("Inequality constraints detected => using BFGS optimizer.")
      if (is.null(control))
        control <- default_BFGS
    }
  } else {
    parsed_constraints <- list(names = NULL, strings = NULL, maxLik = NULL)
    start <- NULL
    
    # Unconstrained optimization
    if (is.null(control)) {
      if (method %in% c("NR", "BHHH")) {
        control <- default_NR
      } else if(method == "NM") {
        control <- default_NM
      } else {
        control <- default_BFGS
      }
    }
  }
  
  # -- 9. Fitting the model with maxLik ----------------------
  ml <- .ml_poisson.fit(y = y,
                        x = x,
                        w = wts_clean,
                        constraints = parsed_constraints$maxLik,
                        start = start,
                        method = method,
                        control = control,
                        ...)
  
  # -- 10. Forming the dataset name ------------------------------
  # Safely get a readable name for the dataset (for printing/storage)
  d_name <- tryCatch(
    deparse(substitute(data)),
    error = function(e) "<unknown data>"
  )
  
  if (length(d_name) > 1 || d_name == "NULL" || grepl("^\\s*\\(", d_name)) {
    d_name <- "<unknown data>"
  }
  
  # -- 11. Internal safety check(s) (for development/testing) --------
  # They should be the same value, since we never indexed sample (that i can remember),
  # so if we get an alert, we must check the code.
  if (length(sample) != n_orig) {
    cli::cli_alert_danger(
      "Internal error: length of 'sample' ({length(sample)}) does not match n_orig ({n_orig})."
    )
  }
  
  # -- 12. Forming the the model list --------------------------------
  
  # -- 12.a. The functions list --------------------------------------
  
  functions <- list(
    predict        = predict.ml_poisson,
    gradientObs    = .ml_poisson_gradientObs,
    hessianObs     = .ml_poisson_hessianObs,
    loglikeObs     = .ml_poisson_loglikeObs,
    loglik         = .ml_poisson_ll,
    fit            = .ml_poisson.fit
  )
  
  # -- 12.b. The common structure --------------------------------------
  model_list <- list(
    description   = "Poisson",
    value         = model_value,
    factor_mapping = factor_mapping,
    formula       = model_value$blueprint$formula,
    weights       = wts_clean,
    w_name        = w_name,
    sample        = sample,
    subset_sample = keep,
    usable_sample = usable_obs,
    data          = data,
    data_name     = d_name,
    functions     = functions,
    response_name = names(model_value$outcomes)[1],
    n_used        = sum(sample),
    n_orig        = n_orig,
    log_info      = log_info,
    control       = control,
    constraints   = parsed_constraints,
    start         = start,
    method        = method
  )
  
  if (!(ml$code %in% c(0, 1, 2, 8))) {
    cli::cli_alert_warning(
      "Estimation did not converge (code {.strong {ml$code}}).\nMessage: {ml$message}"
    )
    cli::cli_alert_info(
      "Returning model without fitted/residual values. Use coef() to inspect parameters."
    )
    model_list$fitted.values <- NULL
    model_list$residuals     <- NULL
    ml$model <- model_list
    return(ml)
  }
  
  # -- 12.c. The fitted values and residuals ------------------------
  # Converged: compute fitted/residuals
  beta <- coef(ml)
  yhat <- as.vector(exp(x %*% beta))
  
  model_list$fitted.values <- yhat
  model_list$residuals     <- y - yhat
  
  # -- 13. Add the model to the maxLik object ----------------------
  ml$model <- model_list
  ml$call <- cl
  
  # -- 14. Call the function to create tge class and return  ----------
  new_ml_poisson(ml)
}

# Hidden function to create the class and return the object.
new_ml_poisson <- function(object, ...) {
  # object is the result from maxLik::maxLik()
  structure(
    object,
    class = unique(c("ml_poisson", "mlmodel.count", "mlmodel", class(object)))
  )
}

# ML_POISSON FIT --------------------------------------------------------------
#' @keywords internal
.ml_poisson.fit <- function(y, x, w = NULL,
                       method = "NR",
                       start = NULL,
                       constraints = NULL,
                       control = list(tol = -1,
                                      reltol = 1e-12,
                                      gradtol = 1e-12,
                                      lambdatol = 1e-20,
                                      qac = "marquardt"),
                       ...)
{
  # At this point start has been checked and if supplied is numeric and
  # has the right dimension.
  if(!is.null(start))
  {
    ll <- .ml_poisson_ll(start, y, x, w)
    
    if(any(!is.finite(ll)))
      cli::cli_abort("Infeasible log-likelihood value at supplied `start` vector.",
                     call = NULL)
    
    names(start) <- paste0("value::", colnames(x))
  }
  else
  {
    # Initial values for Poisson
    mu0  <- (y + mean(y)) / 2
    z    <- log(mu0) + (y - mu0) / mu0
    w_st <- sqrt(mu0) # sqrt because .lm.fit applies weights linearly
    
    # Weighted OLS: solve (X' W X) beta = X' W z
    ols <- .lm.fit(x * w_st, z * w_st)
    b0  <- ols$coefficients

    # Add names for clarity in your summary output later
    names(b0) <- paste0("value::", colnames(x))
    
    start <- .initial_values.mlmodel(.ml_poisson_ll, b0,
                                     y = y, x = x, w = w)
  }
  
  maxLik::maxLik(.ml_poisson_ll,
                 start = start,
                 y = y,
                 x = x,
                 w = w,
                 constraints = constraints,
                 method = method,
                 control = control,
                 ...)
}
