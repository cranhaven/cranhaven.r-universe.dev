## ML_BETA ====================================================================
#' Fit Beta Model by Maximum Likelihood
#'
#' @param value Formula for the conditional log(mean) equation.
#' @param scale Formula for log(phi) equation (precision parameter - optional).
#'  If `NULL`, a homoskedastic (constant precision) model is fitted.
#' @param weights Optional weights variable. It can be either the name of the
#'   variable in `data`, or a vector with the weights.
#' @param data Data frame.
#' @param subset Optional subset expression. Only observations for which this
#'   expression evaluates to `TRUE` are used in the estimation. This can be
#'   a logical vector or an expression (e.g. `subset = age > 30`).
#' @param noint_value Logical. Should the value equation omit the intercept?
#'   Default is `FALSE`.
#' @param noint_scale Logical. Should the scale equation omit the intercept?
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
#' formula (`- 1` or `+ 0`) for the value or scale equations. Use the dedicated
#' arguments `noint_value` and `noint_scale` instead.
#'
#' Coefficient names in the fitted object use the prefixes `value::` and
#' `scale::` to clearly identify to which equation each coefficient belongs to,
#' and to avoid confusion when the same variable(s) appear(s) in both the value
#' and scale equations.
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
#' The Beta model is only defined for a strictly fractional response variable
#' in the open interval (0, 1). Observations where the response is 
#' \code{y <= 0} or \code{y >= 1} are automatically dropped with a warning.
#' If your data contains boundary values (0 or 1), consider using 
#' \code{ml_logit()} instead.
#'
#' @return An object of class `ml_beta` that extends `mlmodel`.
#' 
#' @examples
#' 
#' # Homoskedastic beta regression (fractional response)
#' data(pw401k)
#' 
#' # Beta regression requires 0 < y < 1. 
#' # Observations at the boundaries (0 or 1) are automatically dropped.
#' fit_beta <- ml_beta(prate ~ mrate + I(mrate^2) + log(totemp) + 
#'                     I(log(totemp)^2) + age + I(age^2) + sole, 
#'                     data = pw401k, 
#'                     subset = prate < 1)   # drop y = 1
#' 
#' summary(fit_beta, vcov.type = "robust")
#' 
#' # Heteroskedastic beta regression
#' fit_beta_het <- ml_beta(prate ~ mrate + I(mrate^2) + log(totemp) + 
#'                         I(log(totemp)^2) + age + I(age^2) + sole,
#'                         scale = ~ totemp + sole,
#'                         data = pw401k, 
#'                         subset = prate < 1)
#' 
#' summary(fit_beta_het, vcov.type = "robust")
#' 
#' 
#' # Note: All predictions (including those from predict(), fitted(), and 
#' # residuals()) return values aligned to the original data, with NA 
#' # for observations dropped due to subset or boundary values.
#' 
#' # Different predict types
#' head(predict(fit_beta, type = "response")$fit)   # Expected value E[y]
#' head(predict(fit_beta, type = "variance")$fit)   # Variance of y
#' head(predict(fit_beta, type = "phi")$fit)        # Precision parameter
#' 
#' # Fitted values and residuals
#' head(fitted(fit_beta))
#' head(residuals(fit_beta))
#' head(residuals(fit_beta, type = "pearson"))
#'
#' @author Alfonso Sanchez-Penalver
#'
#' @export
ml_beta <- function(value,
                    scale = NULL,
                    weights = NULL,
                    data,
                    subset = NULL,
                    noint_value = FALSE,
                    noint_scale = FALSE,
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
  
  if (!is.null(scale) && !rlang::is_formula(scale, lhs = FALSE)) {
    cli::cli_abort("`scale` must be a one-sided formula (no outcome on the left-hand side).",
                   call = NULL)
  }
  
  cl <- match.call()
  
  # Making sure we store the formulas in the call and not references to the
  # formulas.
  cl$value <- eval(value)
  
  if(!is.null(scale))
    cl$scale <- eval(scale)
  
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
  if (is.null(scale)) {
    v_vars <- all.vars(value)
  } else {
    v_vars <- unique(c(all.vars(value), all.vars(scale)))
  }
  
  cols_to_check <- v_vars
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
  
  if(log_info$value$is_log)
    cli::cli_abort("Log transformations of the outcome variable are not allowed in the Beta model.")
  
  # Check that observations fall between 0 and 1
  lhs_var <- rlang::f_lhs(value)
  y_vec <- rlang::eval_tidy(lhs_var, data = data)
  # Get indices to keep because of valid y
  good_y_idx <- (y_vec > 1e-12 & y_vec < 1 - 1e-12)
  int_idx <- sample & good_y_idx
  obs_sample <- sum(sample)
  obs_int <- sum(int_idx)
  if(obs_int != obs_sample)
  {
    cli::cli_warn(c(
      "!" = "Dropped {.val {obs_sample - obs_int}} observation(s) at the boundaries (y <= 0 or y >= 1).",
      "i" = "The Beta distribution is only defined on the open interval (0, 1).",
      "*" = "If boundary values are meaningful in your context, consider using {.fn ml_logit} or {.fn ml_probit} instead."
    ))
  }
  
  sample <- int_idx
  
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
  
  if(!is.null(scale))
  {
    model_scale <- hardhat::mold(scale,
                                 data_clean,
                                 blueprint = hardhat::default_formula_blueprint(intercept = !noint_scale))
    
    molds$scale <- model_scale
    
    z <- as.matrix(model_scale$predictors)
  }
  else
  {
    z <- matrix(1, nrow = nrow(x), ncol = 1,
                dimnames = list(NULL, "lnphi"))
    model_scale <- list(
      predictors = tibble::tibble(lnnu = as.vector(z)),
      blueprint = NULL
    )
  }
  
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
    if (length(start) != (ncol(x) + ncol(z)))
      cli::cli_abort("The vector of initial values has the wrong dimension. It requires {.val {ncol(x) + ncol(z)}} values.")
    
    coef_names <- c(paste0("value::", colnames(x)), paste0("scale::", colnames(z)))
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
  ml <- .ml_beta.fit(y = y,
                     x = x,
                     z = z,
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
    predict        = predict.ml_beta,
    gradientObs    = .ml_beta_gradientObs,
    hessianObs     = .ml_beta_hessianObs,
    loglikeObs     = .ml_beta_loglikeObs,
    loglik         = .ml_beta_ll,
    fit            = .ml_beta.fit
  )
  
  # -- 12.b. The common structure --------------------------------------
  model_list <- list(
    description   = if (!is.null(scale)) "Heteroskedastic Beta Model"
                    else "Homoskedastic Beta Model",
    value         = model_value,
    scale         = model_scale,
    factor_mapping = factor_mapping,
    formula       = model_value$blueprint$formula,
    scale_formula = if(!is.null(scale)) model_scale$blueprint$formula else NULL,
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
  # Converged: compute fitted/residuals and ll0 from a constants only fit.
  # z0 <- x0 <- matrix(1, nrow = length(y), ncol = 1)
  # 
  # colnames(x0) <- "(Intercept)"
  # colnames(z0) <- "lnphi"
  # 
  # suppressMessages({
  #   ml0 <- .ml_beta.fit(y = y, x = x0, z = z0, w = wts_clean)
  # })
  # model_list$ll0 <- ml0$maximum
  
  coefs <- coef(ml)
  
  beta <- coefs[1:ncol(x)]
  delta <- coefs[(ncol(x) + 1):length(coefs)]
  yhat <- as.vector(x %*% beta)
  phihat <- as.vector(exp(z %*% delta))
  
  model_list$fitted.values <- exp(yhat)
  model_list$residuals     <- y - exp(yhat)
  model_list$phihat         <- phihat
  
  # -- 13. Add the model to the maxLik object ----------------------
  ml$model <- model_list
  ml$call <- cl
  
  # -- 14. Call the function to create the class and return  ----------
  new_ml_beta(ml)
}

# Hidden function to create the class and return the object.
new_ml_beta <- function(object, ...) {
  # object is the result from maxLik::maxLik()
  structure(
    object,
    class = unique(c("ml_beta", "mlmodel", class(object)))
  )
}

# ML_BETA FIT --------------------------------------------------------------
#' @keywords internal
.ml_beta.fit <- function(y, x, z, w = NULL,
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
    ll <- .ml_beta_ll(start,y,x,z,w)
    
    if(any(!is.finite(ll)))
      cli::cli_abort("Infeasible log-likelihood value at supplied `start` vector.",
                     call = NULL)
    
    names(start) <- c(paste0("value::", colnames(x)),
                      paste0("scale::", colnames(z)))
  }
  else
  {
    # Beta starting values using low-level .ml_logit.fit()
    fit_beta <- .ml_logit.fit(y = y, x = x)
    b0 <- fit_beta$estimate
    names(b0) <- paste0("value::", colnames(x))
    
    # Initial values for nu
    g0 <- rep(0, ncol(z))
    
    # Apply scale:: prefix to all scale coefficients
    names(g0) <- paste0("scale::", colnames(z))
    
    start <- c(b0, g0)
    
    start <- .initial_values.mlmodel(.ml_beta_ll, start,
                                     y = y, x = x, z = z, w = w)
  }
  maxLik::maxLik(.ml_beta_ll,
                 start = start,
                 y = y,
                 x = x,
                 z = z,
                 w = w,
                 constraints = constraints,
                 method = method,
                 control = control,
                 ...)
}