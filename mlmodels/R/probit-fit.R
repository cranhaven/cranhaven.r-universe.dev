## ML_PROBIT ===================================================================
#' Fit Binary Probit Model by Maximum Likelihood
#'
#' @param value Two-sided formula for the probability equation.
#' @param scale Optional one-sided formula for heteroskedasticity.
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
#' formula (`- 1` or `+ 0`) for the value equation. Use the dedicated argument
#' `noint_value` instead. For the scale equation (if modeling heteroskedasticity),
#' the formula must contain only the predictors (right-hand side).
#'
#' The dependent variable must be binary (0/1 or TRUE/FALSE).
#'
#' Coefficient names in the fitted object use the prefixes `value::` and
#' `scale::` (when heteroskedasticity is modeled) to clearly identify which
#' equation each coefficient belongs to.
#' 
#' \code{ml_probit()} handles both strictly binary (`0/1`), and fractional response  
#' (\code{0 <= y <= 1}) outcomes. When using fractional responses, it is recommended
#' to use robust standard errors (\code{vcov.type = "robust"}).

#'
#' Either inequality or equality linear constraints are accepted, but not both.
#' A constraint cannot have a linear combination of more than two coefficients.
#'
#' **Important**: When `constraints` are supplied, `start` cannot be `NULL`.
#' You **must** provide initial values that yield a feasible log-likelihood.
#' If no constraints are used, any supplied `start` is ignored.
#'
#' When constraints are used, `ml_probit` automatically chooses `method`:
#' - Equality constraints => Nelder-Mead (`"NM"`)
#' - Inequality constraints => BFGS (`"BFGS"`)
#'
#' In these cases your supplied `method` argument (if any) is ignored.
#'
#' @return An object of class `ml_probit` that extends `mlmodel`.
#' 
#' @seealso [ml_logit] [ml_beta]
#' 
#' @examples
#' 
#' # Probit model (strictly binary outcome)
#' data(smoke)
#' smoke$smokes <- smoke$cigs > 0
#' 
#' fit_probit <- ml_probit(smokes ~ cigpric + income + age, 
#'                         data = smoke)
#' 
#' summary(fit_probit, vcov.type = "robust")
#' 
#' # Heteroskedastic probit model
#' fit_probit_het <- ml_probit(smokes ~ cigpric + income + age,
#'                             scale = ~ educ,
#'                             data = smoke)
#' 
#' summary(fit_probit_het, vcov.type = "robust")
#' 
#' # Different predict types
#' head(predict(fit_probit, type = "response")$fit)   # Probability of success
#' head(predict(fit_probit, type = "prob0")$fit)      # Probability of failure
#' head(predict(fit_probit, type = "link")$fit)       # Linear predictor (z)
#' 
#' # Fitted values and residuals
#' head(fitted(fit_probit))
#' head(residuals(fit_probit))
#' head(residuals(fit_probit, type = "pearson"))
#'
#' @author Alfonso Sanchez-Penalver
#'
#' @export
ml_probit <- function(value,
                     scale = NULL,
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
  
  # -- 5. Create clean dataset for modeling ----------------------
  data_clean <- data[sample, , drop = FALSE]
  wts_clean <- if (!is.null(wts)) wts[sample] else rep(1, sum(sample))
  
  # Add this safety check:
  if (length(wts_clean) != sum(sample) || any(is.na(wts_clean))) {
    cli::cli_abort("Final weights vector has wrong length or contains NAs.", call = NULL)
  }
  
  # -- 6. Molding ----------------------
  model_value <- hardhat::mold(value,
                               data_clean,
                               blueprint = hardhat::default_formula_blueprint(intercept = !noint_value))
  
  molds <- list(
    value = model_value
  )
  
  y <- as.numeric(model_value$outcomes[[1]])
  
  # -- 7. Validity of outcome variable ------------------
  # Check range
  if (any(y < 0 | y > 1)) {
    cli::cli_abort("Outcome variable must be in the [0, 1] interval.", call = NULL)
  }
  
  # Check if it's purely binary
  is_binary <- all(y %in% c(0, 1))
  
  x <- as.matrix(model_value$predictors)
  
  if(!is.null(scale))
  {
    # Remember Scale has no intercept.
    model_scale <- hardhat::mold(scale,
                                 data_clean,
                                 blueprint = hardhat::default_formula_blueprint(intercept = FALSE))
    
    molds$scale <- model_scale
    
    z <- as.matrix(model_scale$predictors)
  }
  else
    model_scale <- z <- NULL
  
  # -- 8. Map factor variables in relevant equations ------------------
  factor_mapping <- .build_factor_mapping(molds)
  
  # -- 9. Managing control and constraints -------------------
  # Default control lists
  default_NR <- list(tol = -1,
                     reltol = 1e-12,
                     gradtol = 1e-12,
                     lambdatol = 1e-20,
                     qac = "marquardt")
  
  default_BFGS <- list(reltol = 1e-8)
  default_NM <- list(reltol = 1e-8,
                     iterlim = 1000)
  
  # Set control list if user did not provide one
  if (!is.null(constraints)) {
    if (is.null(start))
      cli::cli_abort("Constrained optimization requires a vector of initial values (`start`).",
                     call = NULL)
    if (any(!is.numeric(start)))
      cli::cli_abort("Initial values must be numeric.", call = NULL)
    k <- if (is.null(scale)) ncol(x) else ncol(x) + ncol(z)
    if (length(start) != (k))
      cli::cli_abort("The vector of initial values has the wrong dimension. It requires {.val {ncol(x) + ncol(z)}} values.")
    
    coef_names <- paste0("value::", colnames(x))
    if(!is.null(scale))
      coef_names <- c(coef_names, paste0("scale::", colnames(z)))
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
    # Safety in case of silly users
    if(is.null(method)) method <- "NR"
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
  
  # -- 10. Fitting the model with maxLik ----------------------
  ml <- .ml_probit.fit(y = y,
                      x = x,
                      z = z,
                      w = wts_clean,
                      constraints = parsed_constraints$maxLik,
                      start = start,
                      method = method,
                      control = control,
                      ...)
  
  # -- 11. Forming the dataset name ------------------------------
  # Safely get a readable name for the dataset (for printing/storage)
  d_name <- tryCatch(
    deparse(substitute(data)),
    error = function(e) "<unknown data>"
  )
  
  if (length(d_name) > 1 || d_name == "NULL" || grepl("^\\s*\\(", d_name)) {
    d_name <- "<unknown data>"
  }
  
  # -- 12. Internal safety check(s) (for development/testing) --------
  # They should be the same value, since we never indexed sample (that i can remember),
  # so if we get an alert, we must check the code.
  if (length(sample) != n_orig) {
    cli::cli_alert_danger(
      "Internal error: length of 'sample' ({length(sample)}) does not match n_orig ({n_orig})."
    )
  }
  
  # -- 13. Forming the model list ------------------------------------
  
  # -- 13.a. The functions list --------------------------------------
  
  functions <- list(
    predict        = predict.ml_probit,
    gradientObs    = .ml_probit_gradientObs,
    hessianObs     = .ml_probit_hessianObs,
    loglikeObs     = .ml_probit_loglikeObs,
    loglik         = .ml_probit_ll,
    fit            = .ml_probit.fit
  )
  
  model_desc <- {
    if(!is.null(scale))
    {
      # Hetero
      if(is_binary) "Heteroskedastic Binary Probit"
      else "Heteroskedastic Fractional Response Probit"
    }
    else
    {
      # Homo
      if(is_binary) "Homoskedastic Binary Probit"
      else "Homoskedastic Fractional Response Probit"
    }
  }
  
  # -- 13.b. The model_list list --------------------------------------
  model_list <- list(
    description   = model_desc,
    value         = model_value,
    scale         = model_scale,
    factor_mapping  = factor_mapping %||% list(value = list()),
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
    log_info      = NULL,
    control       = control,
    constraints   = parsed_constraints,
    start         = start,
    method        = method,
    is_binary     = is_binary
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
  
  # Converged: compute fitted (easy calculation of pseudo R-squared)]
  cfs <- coef(ml)
  beta <- cfs[1:ncol(x)]
  xb <- x %*% beta
  if(!is.null(scale))
  {
    delta <- cfs[(ncol(x)+1):length(cfs)]
    sig <- as.vector(exp(z %*% delta))
  }
  else
    sig <- 1
  model_list$fitted.values <- as.vector(pnorm(xb / sig))
  model_list$residuals <- y - model_list$fitted.values
  model_list$sigma <- sig
  
  # -- 14. Add the model to the maxLik object ----------------------
  ml$model <- model_list
  ml$call <- cl
  
  # -- 15. Call the function to create tge class and return  ----------
  new_ml_probit(ml)
}

# Hidden function to create the class and return the object.
new_ml_probit <- function(object, ...) {
  # object is the result from maxLik::maxLik()
  structure(
    object,
    class = unique(c("ml_probit", "mlmodel", class(object)))
  )
}

## ML_PROBIT FIT ===============================================================
#' @keywords internal
.ml_probit.fit <- function(y, x, z = NULL, w = NULL,
                          constraints = NULL,
                          start = NULL,
                          method = NULL,
                          control = NULL,
                          ...)
{
  n <- length(y)
  
  # Starting values
  if (!is.null(start)) {
    # User provided start (required when constraints are used)
    ll <- .ml_probit_ll(start, y = y, x = x, z = z, w = w)
    if (any(!is.finite(ll)))
      cli::cli_abort("Infeasible log-likelihood value at supplied `start` vector.",
                     call = NULL)
    
    # Name the coefficients properly
    names(start) <- c(paste0("value::", colnames(x)),
                      if (!is.null(z)) paste0("scale::", colnames(z)) else character(0))
    
  } else {
    # Default starting values for unconstrained logit
    p <- mean(y)
    
    has_constant <- (ncol(x) > 0 && isTRUE(all(x[, 1] == 1)))
    
    # Initial values.
    ols <- .lm.fit(x, y)
    b0 <- ols$coefficients
    
    # Amemiya's scaling.
    if(has_constant)
    {
      b0[-1] <- 2.5 * b0[-1]
      b0[1] <- 2.5 * (b0[1] - 0.5)
    }
    else
      b0 <- 2.5 * b0
    names(b0) <- paste0("value::", colnames(x))
    
    if (!is.null(z)) {
      g0 <- rep(0, ncol(z))
      names(g0) <- paste0("scale::", colnames(z))
      start_values <- c(b0, g0)
    } else {
      # Homoskedastic case
      start_values <- b0
    }
    start <- .initial_values.mlmodel(.ml_probit_ll, start_values,
                                     y = y, x = x, z = z, w = w)
  }
  
  # Final estimation
  maxLik::maxLik(.ml_probit_ll,
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