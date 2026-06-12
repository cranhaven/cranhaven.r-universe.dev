# mlmodels: Exported utility functions for all mlmodel objects

## AIC =========================================================================
#' Extract AIC from mlmodel objects
#'
#' @param object An object of class `"mlmodel"` or `"summary.mlmodel"`.
#' @param k Numeric. The penalty per parameter. Default is `k = 2` 
#'   (standard AIC). See [stats::AIC()] for details.
#' @param ... Further arguments passed to methods.
#'
#' @details
#' For `mlmodel` objects, AIC is computed as `-2 * logLik(object) + k * npar`.
#' 
#' For `summary.mlmodel` objects, the pre-computed AIC (with `k = 2`) is 
#' returned; the `k` argument is accepted for compatibility but ignored.
#'
#' @return A numeric value with the AIC.
#'
#' @method AIC mlmodel
#' @export
AIC.mlmodel <- function(object, ..., k = 2)
{
  if (!inherits(object, "mlmodel"))
    cli::cli_abort("`object` must inherit from class 'mlmodel'.", call = NULL)
  
  if (!(object$code %in% c(0L, 1L, 2L, 8L))) {
    cli::cli_abort("AIC is not available (model did not converge).", call = NULL)
  }
  
  ll <- logLik(object)
  npar <- attr(ll, "df") %||% length(coef(object))
  
  -2 * as.numeric(ll) + k * npar
}

# --- summary.mlmodel ----------------------------------------------------------
#' @rdname AIC.mlmodel
#' @export
AIC.summary.mlmodel <- function(object, ..., k = 2)
{
  if (!inherits(object, "summary.mlmodel"))
    cli::cli_abort("`object` must inherit from class 'summary.mlmodel'.", call = NULL)
  
  if (is.null(object$AIC) || !isTRUE(object$converged))
    cli::cli_abort("AIC is not available (model did not converge).", call = NULL)
  
  # Note: we ignore the `k` argument for summary objects because we already computed AIC with k=2
  object$AIC
}

## BIC =========================================================================
#' Extract BIC from mlmodel objects
#'
#' @param object An object of class `"mlmodel"` or `"summary.mlmodel"`.
#' @param ... Further arguments passed to methods.
#'
#' @details
#' BIC is computed as `-2 * logLik(object) + log(nobs) * npar`.
#'
#' @return A numeric value with the BIC.
#'
#' @method BIC mlmodel
#' @export
BIC.mlmodel <- function(object, ...)
{
  if (!inherits(object, "mlmodel"))
    cli::cli_abort("`object` must inherit from class 'mlmodel'.", call = NULL)
  
  if (!(object$code %in% c(0L, 1L, 2L, 8L))) {
    cli::cli_abort("AIC is not available (model did not converge).", call = NULL)
  }
  
  ll <- logLik(object)
  npar <- attr(ll, "df") %||% length(coef(object))
  nobs <- attr(ll, "nobs") %||% object$model$n_used
  
  -2 * as.numeric(ll) + log(nobs) * npar
}

# --- summary.mlmodel ----------------------------------------------------------
#' @rdname BIC.mlmodel
#' @export
BIC.summary.mlmodel <- function(object, ...)
{
  if (!inherits(object, "summary.mlmodel"))
    cli::cli_abort("`object` must inherit from class 'summary.mlmodel'.", call = NULL)
  
  if (is.null(object$BIC))
    cli::cli_abort("BIC is not available (model did not converge).", call = NULL)
  
  object$BIC
}

## COEFFICIENTS ================================================================
#' Extract Model Coefficients
#'
#' @param object An `mlmodel` object.
#' @param ... Currently not used.
#'
#' @return A named numeric vector of estimated coefficients.
#'
#' @method coef mlmodel
#' @export
coef.mlmodel <- function(object, ...) {
  if (!inherits(object, "mlmodel"))
    cli::cli_abort("`object` must be a model of class 'mlmodel'.",
                   call = NULL)
  object$estimate
}

## CONFINT =====================================================================
#' Confidence Intervals for mlmodel Coefficients
#'
#' @param object An `mlmodel` object.
#' @param parm A specification of which parameters are to be given confidence 
#'   intervals (names or numeric indices). If missing, all parameters are used.
#' @param level The confidence level required. Default is 0.95.
#' @param vcov Optional user-supplied variance-covariance matrix.
#' @param vcov.type Type of variance-covariance matrix to use. 
#'   See [vcov.mlmodel].
#' @param cl_var Clustering variable (name as string or vector).
#' @param repetitions Number of bootstrap replications when `vcov.type = "boot"`.
#' @param seed Random seed for bootstrap/jackknife.
#' @param progress Show progress bar? Default `FALSE`.
#' @param ... Further arguments passed to methods.
#'
#' @returns Matrix with the confidence intervals for the requested parameters.
#' 
#' @details
#' Confidence intervals are constructed as 
#' \eqn{\hat{\beta} \pm z_{1-\alpha/2} \times SE(\hat{\beta})}, 
#' where the standard errors come from the requested variance-covariance matrix.
#' 
#' The function supports all variance types available in [vcov.mlmodel], 
#' including robust, clustered, bootstrap, and jackknife estimators.
#'
#' @examples
#' 
#' data(mroz)
#' mroz$incthou <- mroz$faminc / 1000
#' 
#' fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, 
#'              data = mroz)
#' 
#' # Default 95% confidence intervals (using OIM)
#' confint(fit)
#' 
#' # 90% confidence intervals
#' confint(fit, level = 0.90)
#' 
#' # Confidence intervals for specific parameters
#' confint(fit, parm = c("value::educ", "value::huswage"))
#' confint(fit, parm = 4:5)                     # by position
#' 
#' # Using different variance types
#' confint(fit, vcov.type = "robust")
#' 
#' # Clustered confidence intervals
#' confint(fit, vcov.type = "robust", cl_var = "age")
#' 
#' # Using a pre-computed bootstrap variance matrix
#' v_boot <- vcov(fit, type = "boot", repetitions = 100, seed = 123)
#' confint(fit, vcov = v_boot)
#'
#' @method confint mlmodel
#' @export
confint.mlmodel <- function(object,
                            parm,
                            level = 0.95,
                            vcov = NULL,
                            vcov.type = "oim",
                            cl_var = NULL,
                            repetitions = 999,
                            seed = NULL,
                            progress = FALSE,
                            ...)
{
  if (!inherits(object, "mlmodel"))
    cli::cli_abort("`object` must be a model of class 'mlmodel'.", call = NULL)
  
  coefs <- coef(object)
  
  if(missing(parm))
    cfs_idx <- rep(TRUE, length(coefs))
  else
  {
    if (is.numeric(parm))
    {
      # indices
      if (any(is.na(parm)) || any(parm > length(coefs)) || any(parm < 1) ||
          any(parm != floor(parm)))
        cli::cli_abort("Invalid indices in `parm`.", call = NULL)
      cfs_idx <- seq_along(coefs) %in% parm
    }
    else
    {
      if(!is.character(parm))
        cli::cli_abort("Invalid format for `parm`.", call = NULL)
      cfs_idx <- names(coefs) %in% parm
    }
  }
  # This could happen if the names in parm are not in the coefficient names, but
  # it is safer to do it out of the if statement in case I missed something in
  # the is.numeric part.
  if (!any(cfs_idx))
    cli::cli_abort("Invalid parameters in `parm`.", call = NULL)
  
  # Preliminary values.
  a <- (1 - level) / 2
  crit <- qnorm(c(a, 1 - a))
  parm_names <- names(coefs)[cfs_idx]
  pct <- paste(format(100 * c(a, 1 - a), trim = TRUE, scientific = FALSE, digits = 3), "%")
  
  # Process the vcov
  vcov <- .process_vcov(object,
                        vcov = vcov,
                        vcov.type = vcov.type,
                        cl_var = cl_var,
                        repetitions = repetitions,
                        seed = seed,
                        progress = progress)
  
  
  # -- Check for unusable variance matrix --------------------------------------
  if (any(!is.finite(vcov)) || any(is.na(vcov))) {
    cli::cli_warn(
      c("Variance matrix is unusable (contains NAs or non-finite values).",
        "i" = "This usually happens with bootstrap when constraints are present.",
        "i" = "Standard errors will be returned as NA.")
    )
    ci_matrix <- matrix(NA, nrow = length(parm_names), ncol = 2)
  }
  else
  {
    ses <- sqrt(diag(vcov))
    ci_matrix <- coefs + ses %o% crit
    ci_matrix <- ci_matrix[cfs_idx, , drop = FALSE]
  }
  rownames(ci_matrix) <- parm_names
  colnames(ci_matrix) <- pct
  
  ci_matrix
}

## FITTED ======================================================================
#' Extract Fitted Values from mlmodel
#'
#' @param object An `mlmodel` object.
#' @param ... Further arguments passed to methods (currently ignored).
#'
#' @return A numeric vector of fitted values, aligned to the original data.
#'   Dropped observations (due to `NA`s or `subset`) return `NA`.
#'
#' @method fitted mlmodel
#' @export
fitted.mlmodel <- function(object, ...) {
  
  if (!inherits(object, "mlmodel")) {
    cli::cli_abort("`object` must be of class 'mlmodel'.")
  }
  
  # Use predict() so we get the correctly aligned version with NAs
  predict(object, type = "response")$fit
}

#' @rdname fitted.mlmodel
#' @export
fitted.values.mlmodel <- fitted.mlmodel

## FORMULA =====================================================================
#' Extract value formula from mlmodel objects
#' 
#' @param x An `mlmodel` object
#' @param ... Currently not implemented
#' 
#' @returns The formula of the value equation (object of class `formula`).
#'
#' @export
formula.mlmodel <- function(x, ...) {
  # Return the main (value) formula
  if (!is.null(x$model$formula)) {
    x$model$formula
  } else if (!is.null(x$model$value$blueprint$formula)) {
    x$model$value$blueprint$formula
  } else {
    NULL
  }
}

## LOGLIK ======================================================================
# --- General ------------------------------------------------------------------
#' Extract Log-Likelihood from mlmodel objects
#'
#' @param object An object of class `mlmodel` or `summary.mlmodel`.
#' @param ... Additional arguments passed to methods.
#'
#' @details
#' The returned object is of class `"logLik"` and has two important attributes:
#' 
#' * `nobs`: number of observations used in estimation.
#' * `df`: number of estimated parameters (usually called *K*), 
#'   computed as `length(coef(object))`. This includes coefficients from both 
#'   the location (mean/value) and scale equations when present.
#'
#' @return An object of class `"logLik"` with the log-likelihood value and the 
#'   attributes `nobs` and `df`.
#'
#' @method logLik mlmodel
#' @export
logLik.mlmodel <- function(object, ...)
{
  if (!inherits(object, "mlmodel"))
    cli::cli_abort("`object` must inherit from class 'mlmodel'.",
                   call = NULL)
  
  ll <- object$maximum
  
  # Attach useful attributes
  attr(ll, "nobs") <- object$model$n_used
  attr(ll, "df")   <- length(coef(object))   # number of free parameters
  
  ll
}

# --- summary.mlmodel ----------------------------------------------------------
#' @rdname logLik.mlmodel
#' @export
logLik.summary.mlmodel <- function(object, ...)
{
  if(!inherits(object, "summary.mlmodel"))
    cli::cli_abort("`object` must inerit from class `summary.mlmodel`.",
                   call = NULL)
  if (is.null(object$logLik) || is.na(object$logLik)) {
    cli::cli_warn("Log-likelihood value is not available.")
    return(NA_real_)
  }
  
  ll <- object$logLik
  
  attr(ll, "nobs") <- object$nobs
  attr(ll, "df") <- nrow(object$coefficients)
  
  ll
}

## MLE FUNCTIONS ===============================================================
# -- gradientObs ----------------------------------------------------------------
#' Gradient (Score) by Observation
#'
#' Extract the per-observation gradients (scores) evaluated at the estimated
#' parameters from an `mlmodel` object.
#'
#' @param object An `mlmodel` object.
#'
#' @return A numeric matrix with one row per observation and one column per 
#'   parameter. Each row contains the gradient of the log-likelihood for that 
#'   observation.
#'
#' @details
#' These are the individual contributions to the score vector. They are mainly 
#' useful for advanced users who want to implement custom tests or diagnostics.
#'
#' @export
gradientObs <- function(object) UseMethod("gradientObs")

#' @rdname gradientObs
#' @export
gradientObs.mlmodel <- function(object)
{
  if(!inherits(object, "mlmodel"))
    cli::cli_abort("`object` needs to be of class `'mlmodel'`")
  object$model$functions$gradientObs(object)
}

# -- hessianObs ----------------------------------------------------------------
#' Hessian by Observation
#'
#' Extract the per-observation Hessian matrices evaluated at the estimated
#' parameters from an `mlmodel` object.
#'
#' @param object An `mlmodel` object.
#'
#' @return A numeric matrix of dimension `(N*K) x K`, where `N` is the number 
#'   of observations and `K` is the number of parameters. The Hessian for each 
#'   observation is stacked vertically.
#'
#' @details
#' This is mainly intended for advanced use (e.g., custom diagnostics or 
#' information matrix tests). For most users, the functions `IMtest` or 
#' `vcov` are more convenient.
#'
#' @export
hessianObs <- function(object)
  UseMethod("hessianObs")

#' @rdname hessianObs
#' @export
hessianObs.mlmodel <- function(object)
{
  if(!inherits(object, "mlmodel"))
    cli::cli_abort("`object` needs to be of class `'mlmodel'`")
  object$model$functions$hessianObs(object)
}

# -- loglikeObs ----------------------------------------------------------------
#' Log-Likelihood by Observation
#'
#' Extract the per-observation log-likelihood contributions from an `mlmodel` 
#' object.
#'
#' @param object An `mlmodel` object.
#'
#' @return A numeric vector of length `nobs(object)` containing the 
#'   log-likelihood contribution of each observation.
#'
#' @details
#' These individual contributions are useful for Vuong tests, robust variance 
#' estimation, or custom model diagnostics.
#'
#' @export
loglikeObs <- function(object)
  UseMethod("loglikeObs")

#' @rdname loglikeObs
#' @export
loglikeObs.mlmodel <- function(object)
{
  if(!inherits(object, "mlmodel"))
    cli::cli_abort("`object` needs to be of class `'mlmodel'`")
  object$model$functions$loglikeObs(object)
}

# NOBS =========================================================================
#' Extract the Number of Observations from an mlmodel
#'
#' @param object An object of class `"mlmodel"`.
#' @param ... Further arguments passed to methods (currently not used).
#'
#' @return An integer giving the number of observations used in the estimation 
#'   (after removing missing values and applying any `subset`).
#'
#' @method nobs mlmodel
#' @export
nobs.mlmodel <- function(object, ...) {
  if(!inherits(object, "mlmodel"))
    cli::cli_abort("`object` must be of class `'mlmodel'`",
                   call = NULL)
  n <- object$model$n_used %||% 
    object$nobs %||% 
    length(object$model$sample) %||% 
    length(object$model$usable_sample)
  
  if (is.null(n) || n == 0) {
    cli::cli_warn("Could not determine number of observations.")
    return(NA_integer_)
  }
  
  as.integer(n)
}

## NULL DEFAULT ================================================================
#' Null default operator
#'
#' @description
#' Provides a convenient infix operator for replacing `NULL` values.
#' `x \%||\% y` is equivalent to `if (is.null(x)) y else x`.
#'
#' @param x,y Any R objects.
#'
#' @name null-default
#' @usage x \%||\% y
#' 
#' @returns 
#' The value of `x` if it is not `NULL`, otherwise the value of `y`.
#' 
#' @examples
#' NULL %||% "fallback"
#' list(a = 1) %||% list(b = 2)
#' @export
`%||%` <- rlang::`%||%`

## PREDICT GENERIC =============================================================
#' Predictions for mlmodel models
#'
#' Methods for computing predictions from models fitted with the
#' `mlmodels` package.
#' 
#' @param object An object from an estimation with one of our models.
#' @param newdata Optional data frame for out-of-sample predictions.
#' @param type Character string indicating what to predict. See **Details**.
#' @param se.fit Logical. If `TRUE`, also return standard errors (delta method).
#' @param vcov Optional user-supplied variance-covariance matrix.
#' @param vcov.type Type of variance-covariance matrix. See [vcov][vcov.mlmodel].
#' @param cl_var Clustering variable (name or vector).
#' @param repetitions Number of bootstrap replications when `vcov.type = "boot"`.
#' @param seed Random seed for bootstrapping, for reproducibility.
#' @param progress Logical. Show bootstrap/jackknife progress bar? Default is
#'    `FALSE` in higher-level functions.
#' @param ... Additional arguments passed to methods.
#'
#' @returns An object that inherits from `predict.mlmodel` and has two elements:
#' \describe{
#'    \item{fit}{Vector with the predictions.}
#'    \item{se.fit}{If `se.fit` is `TRUE` a vector with the delta-method standard
#'    errors, using analytical gradients. If `se.fit` is `FALSE`, it is set to
#'    `NULL`.}
#' }
#' 
#' @examples
#' 
#' # Basic usage and different predict types
#' data(docvis)
#' fit_pois <- ml_poisson(docvis ~ age + educyr + totchr, data = docvis)
#' 
#' head(predict(fit_pois, type = "response")$fit)     # Expected count
#' head(predict(fit_pois, type = "P(3)")$fit)         # Prob of exactly 3
#' 
#' # Prediction at the mean (typical case)
#' typical <- data.frame(age = mean(docvis$age), 
#'                       educyr = mean(docvis$educyr), 
#'                       totchr = mean(docvis$totchr))
#' predict(fit_pois, newdata = typical, type = "response")
#' 
#' # In-sample vs full-data prediction with subset / boundary dropping
#' data(pw401k)
#' fit_beta <- ml_beta(prate ~ mrate + I(mrate^2) + log(totemp) + 
#'                     I(log(totemp)^2) + age + I(age^2) + sole,
#'                     data = pw401k, 
#'                     subset = prate < 1)
#' 
#' # In-sample prediction (NAs for dropped observations)
#' head(predict(fit_beta, type = "response")$fit)
#' 
#' # Full-data prediction (predicts for all rows, including dropped ones)
#' head(predict(fit_beta, newdata = pw401k, type = "response")$fit)
#' 
#' @author Alfonso Sanchez-Penalver
#' 
#' @method predict mlmodel
#' @aliases predict.mlmodel
#' @export
predict.mlmodel <- function(object, ...) {
  UseMethod("predict")
}

## RESIDUALS ===================================================================
#' Extract Model Residuals
#'
#' @param object An `mlmodel` object.
#' @param type Character string. Type of residuals to return. 
#'   Currently supported: `"response"` (default) or `"pearson"`.
#' @param ... Further arguments passed to methods (currently not used).
#'
#' @details
#' `"response"` residuals are the raw residuals: observed minus fitted values.
#' 
#' `"pearson"` residuals are standardized by the model-implied standard deviation:
#' \eqn{(y - \hat{y}) / \sqrt{\text{Var}(y)}}. 
#' For Poisson models they use \eqn{\sqrt{\hat{\mu}}}, for binary models 
#' \eqn{\sqrt{\hat{p}(1-\hat{p})}}, and for other models the appropriate 
#' variance from `predict(object, type = "var")` or `type = "var_y"`.
#'
#' @return A numeric vector of residuals aligned to the original data frame. 
#'   Observations dropped during estimation (due to `NA`s or `subset`) 
#'   return `NA`.
#'
#' @method residuals mlmodel
#' @export
residuals.mlmodel <- function(object, type = c("response", "pearson"), ...)
{
  if(!inherits(object, "mlmodel"))
    cli::cli_abort("`object` needs to be of class `'mlmodel'`", call = NULL)
  
  type <- rlang::arg_match(type)
  
  # No matter the type we need the response prediction.
  
  fitted_values <- predict(object)$fit  # response is the default prediction for all models
  
  y <- .reconstruct_full_sample_vector(object$model$value$outcomes[[1]],
                                       object$model$sample)
  resid <- y - fitted_values
  
  if(type == "response") return(resid)
  
  # Models to check:
  #   - ml_lm: if it's not lognormal prediction type is "var", if lognormal "var_y".
  #   - ml_poisson: var = mu
  #   - ml_probit and ml_logit: var  = mu * (1 - mu)
  
  if(inherits(object, "ml_lm"))
  {
    if(object$model$log_info$value$is_log)
      var <- predict(object, type = "var_y")$fit
    else
      var <- predict(object, type = "var")$fit
  }
  else if(inherits(object, "ml_poisson"))
    var <- fitted_values
  else if(inherits(object, "ml_logit") || inherits(object, "ml_probit"))
    var <- fitted_values * (1 - fitted_values)
  else
    var <- predict(object, type = "var")$fit
  
  pear <- resid / sqrt(var)
  
  return(pear)
}

## SE ==========================================================================
# --- Generic ------------------------------------------------------------------
#' Extract Standard Errors from mlmodel Objects
#'
#' @param object An object of class `"mlmodel"`.
#' @param vcov An optional user-supplied variance-covariance matrix.
#' @param vcov.type Character string specifying the type of variance-covariance
#'   matrix to use. One of `"oim"` (default), `"opg"`, `"robust"`, `"boot"`,
#'   or `"jack"`. See [vcov.mlmodel()] for details.
#' @param cl_var Character string or vector. Name of the clustering variable
#'   (or the vector itself) when `vcov.type = "robust"` (or its alias `"cluster"`).
#' @param repetitions Integer. Number of bootstrap replications when 
#'   `vcov.type = "boot"`. Default is 999.
#' @param seed Integer. Random seed for reproducibility when bootstrapping.
#'   If `NULL`, a random seed is generated internally.
#' @param progress Logical. Should a progress bar be shown during bootstrapping
#'   or jackknifing? Default is `FALSE`.
#' @param ... Further arguments passed to methods (currently not used).
#'
#' @return A named numeric vector of standard errors, with the same names 
#'   as `coef(object)`.
#'
#' @seealso [vcov.mlmodel], [summary.mlmodel], [confint.mlmodel]
#'
#' @export
se <- function(object, ...) {
  UseMethod("se")
}

# --- mlmodel ------------------------------------------------------------------
#' Extracts standard errors for an `mlmodel` object.
#' 
#' @rdname se
#' @export
se.mlmodel <- function(object,
                       vcov = NULL,
                       vcov.type = "oim",
                       cl_var = NULL,
                       repetitions = 999,
                       seed = NULL,
                       progress = FALSE,
                       ...)
{
  if (!inherits(object, "mlmodel"))
    cli::cli_abort("`object` must be of 'mlmodel' class.")

  var <- .process_vcov(object,
                       vcov = vcov,
                       vcov.type   = vcov.type,
                       cl_var      = cl_var,
                       repetitions = repetitions,
                       seed        = seed,
                       progress    = progress)
  se <- sqrt(diag(var))
  names(se) <- colnames(var)
  return(se)
}

## SUMMARY GENERIC =============================================================
#' Summary for mlmodel objects
#'
#' @param object A fitted model object of class `"mlmodel"`.
#' @param correlation Logical. Should the correlation matrix of the estimated
#'   parameters be included in the output? Default is `FALSE`. If `TRUE` the
#'   correlation matrix will be computed, and stored in the `'summary.mlmodel'`
#'   object the function returns.
#' @param vcov Optional user-supplied variance-covariance matrix. If provided,
#'   it will be used instead of computing one internally.
#' @param vcov.type Character string specifying the type of variance-covariance
#'   matrix to use. See [vcov][mlmodels::vcov.mlmodel].
#' @param cl_var Character string or vector. Name of the clustering variable
#'   or the vector itself. See [vcov][mlmodels::vcov.mlmodel].
#' @param repetitions Integer. Number of bootstrap replications when
#'   `vcov.type = "boot"`. Default is 999.
#' @param seed Integer. Random seed for reproducibility when `vcov.type = "boot"`.
#'   If `NULL`, a random seed is generated.
#' @param progress Logical. Should a progress bar be displayed during
#'   bootstrapping or jackknifing? Default is `FALSE` (silent).
#' @param ... Further arguments passed to methods.
#'
#' @details
#' Coefficient names in the fitted object use the prefixes `value::` and
#' `scale::` to identify to which equation they belong to, and to avoid
#' confusion when the same variable(s) appear(s) in both the value and scale
#' equations.
#' 
#' @returns An object of class `summary.mlmodel` and any other class that extends
#'    it.
#' 
#' @examples
#' 
#' data(mroz)
#' mroz$incthou <- mroz$faminc / 1000
#' 
#' fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, 
#'              data = mroz)
#' 
#' # Default: observed information matrix
#' summary(fit)
#' 
#' # Different variance types
#' summary(fit, vcov.type = "opg")                    # Outer product of gradients
#' summary(fit, vcov.type = "robust")                 # Robust/sandwich estimator
#' 
#' # Clustered robust standard errors
#' summary(fit, vcov.type = "robust", cl_var = "age")
#' 
#' # Using a pre-computed variance matrix (e.g. bootstrap)
#' v_boot <- vcov(fit, type = "boot", repetitions = 100, seed = 123)
#' summary(fit, vcov = v_boot)
#' 
#' @author Alfonso Sanchez-Penalver
#' 
#' @method summary mlmodel
#' @export
summary.mlmodel <- function(object,
                            correlation = FALSE,
                            vcov = NULL,           # User-supplied variance matrix
                            vcov.type = "oim",
                            cl_var = NULL,
                            repetitions = 999,
                            seed = NULL,
                            progress = FALSE,
                            ...)
{
  UseMethod("summary")
}

## UPDATE GENERIC ==============================================================
#' Update an mlmodel Call
#'
#' @param object An `mlmodel` object.
#' @param formula. An updated formula for the value (location/mean) equation.
#' @param scale. An updated formula for the scale equation (if supported by the model).
#' @param data A data frame to be used when re-fitting the model.
#' @param weights Optional case weights.
#' @param subset An expression or logical vector to subset, or a vector of indices
#'   to resample (`sandwich` uses it for that).
#' @param evaluate Logical. If `TRUE` (default), the updated call is evaluated 
#'   and the new fitted model is returned. If `FALSE`, the updated call 
#'   (as a language object) is returned without evaluation.
#' @param ... Further arguments passed to methods (currently ignored).
#'
#' @details
#' This method re-evaluates the original model call after modifying selected 
#' arguments. It is used internally by `IMtest()` and serves as a fallback 
#' mechanism in bootstrap and jackknife variance estimation when a model-specific 
#' implementation is not available.
#'
#' @details
#' **`sandwich` package compatibility**
#' 
#' The functions `sandwich::vcovBS()` and `sandwich::vcovJK()` are supported
#' through this `update()` method. They produce numerically equivalent results
#' to our own `vcov(object, type = "boot")` and `vcov(object, type = "jack")`
#' when all bootstrap/jackknife replications converge, taking longer to compute
#' them.
#' 
#' **Important difference**: When some replications fail to converge,
#' `sandwich` includes those failed iterations in the variance calculation,
#' while our `vcov()` implementation uses **only successful replications**.
#' The latter is statistically more appropriate.
#' 
#' We therefore strongly recommend using the native `vcov()` methods provided
#' by **mlmodels** for bootstrap and jackknife variance-covariance matrices.
#' 
#' @returns And updated `mlmodel` object (or the class of the estimator that
#'    extends it) with the modified formula/call and refitted parameters.
#'
#' @method update mlmodel
#' @export
update.mlmodel <- function(object,
                           formula. = NULL,
                           scale. = NULL,
                           data = NULL,
                           weights = NULL,
                           subset = NULL,
                           ...,
                           evaluate = TRUE)
{
  if (is.null(call <- object$call))
    cli::cli_abort("`object` does not contain a `call` component.", call = NULL)
  
  # Update value formula if explicitly requested (rare for bootstrap)
  if (!is.null(formula.)) {
    call$value <- update.formula(formula(object), formula.)
  }
  
  # Update scale formula if explicitly requested
  if (!is.null(scale.)) {
    if (identical(scale., ~1) || identical(scale., ~0)) {
      call$scale <- NULL
    } else {
      call$scale <- scale.
    }
  }
  # If scale. is not supplied -> keep the original scale formula (crucial for heteroskedastic models)
  
  # Update data (the main thing vcovBS passes)
  if (!is.null(data)) {
    call$data <- data
  }
  
  # Update weights
  if (!is.null(weights)) {
    call$weights <- weights
  }
  
  # Process Subset (sandwich uses a vector fo indices in it to resample data)
  if (!is.null(subset) && is.numeric(subset) && !all(subset %in% c(0L, 1L))) {
    
    # This is a vector of indices of the rows from the estimated data that
    # have to be in the new estimation data (resampling).
    
    # We pull the original dataset and index it to reduce it to the observations
    # used in the original estimation.
    est_data <- object$model$data[object$model$sample, , drop = FALSE]
    
    if (min(subset) < 1 || max(subset) > nrow(est_data)) {
      cli::cli_abort("Invalid indices in `subset` vector.")
    }
    
    # If bootstrapping the vector will have a length equal to the number of
    # observations in est_data, but if jackknifing it will be the number of
    # observations minus one (the one dropped out).
    est_n <- nrow(est_data)
    
    if (length(subset) == est_n) {
      # Bootstrapping. We just index est_data with the vector to form the new
      # dataframe.
      new_data <- est_data[subset, , drop = FALSE]
      
    } else if (length(subset) == est_n - 1) {
      # Jackknifing, we form a logical vector with length equal to est_n that will
      # end up having FALSE in the observation that was dropped.
      full_idx <- rep(FALSE, est_n)
      full_idx[subset] <- TRUE        # subset = indices to KEEP
      # And now we index est_data to return all observations except the FALSE one.
      new_data <- est_data[full_idx, , drop = FALSE]
    } else {
      cli::cli_abort("`subset` vector length ({length(subset)}) is invalid for bootstrap or jackknife.")
    }
    
    call$data   <- new_data      # Pass the resampled data
    # Since we are passing a dataframe with only data used in the original estimation
    # we must set the subset argument in the call to NULL, in case the original
    # estimation was done with a subset condition, because the dataset has already
    # been subset.
    call$subset <- NULL
  }
  else if (!is.null(subset)) {
    # If subset is not a vector of integers different from 0 and 1, then it must
    # be a true subset condition: we modify the argument in the call
    call$subset <- subset
  }
  
  # Forward any other arguments the user might pass explicitly
  extras <- match.call(expand.dots = FALSE)$...
  if (length(extras) > 0) {
    for (arg in names(extras)) {
      call[[arg]] <- extras[[arg]]
    }
  }
  
  # Evaluate or return the updated call
  if (evaluate) {
    eval(call, envir = parent.frame())
  } else {
    call
  }
}

## VARIANCE ====================================================================
#' Variance-Covariance Matrix for mlmodel Objects
#'
#' Returns the variance-covariance matrix of the estimated parameters
#' using different methods.
#'
#' @param object An object of class `"mlmodel"` or a model inheriting from it
#'   (e.g. `"ml_lm"`).
#' @param type Character string specifying the type of variance-covariance
#'   matrix. One of `"oim"` (default), `"robust"`, `"opg"`, `"cluster"`,
#'   `"boot"`, and `"jack"` or `"jackknife"`.
#' @param cl_var Character string or vector. Name of the clustering variable
#'   in the data, or the vector itself.
#' @param repetitions Integer. Number of bootstrap replications to use when
#'   `type = "boot"`. Default is 999.
#' @param seed Integer. Random seed for reproducibility when `type = "boot"`.
#'   If `NULL`, a random seed is generated.
#' @param progress Logical. Should a progress bar be displayed? Default is
#'   `TRUE` when `type` is `"boot"` or `"jack"`/`"jackknife"`. Ignored for other
#'   types.
#' @param ... Further arguments passed to methods.
#'
#' @return A symmetric variance-covariance matrix with coefficient names
#'   on the rows and columns.
#' 
#' @details
#' The package provides several variance-covariance estimators through the `type` argument:
#' 
#' * `"oim"` - Observed Information Matrix (default)
#' * `"opg"` - Outer Product of Gradients (BHHH)
#' * `"robust"` - Robust (sandwich) estimator
#' * `"cluster"` - Alias for `"robust"` when clustering the variance but requires `cl_var` to be set.
#' * `"boot"` - Bootstrap (with optional clustering)
#' * `"jack"` - Jackknife (with optional clustering)
#' * `"jackknife"` - alias for `"jack"`
#' 
#' Clustered standard errors are obtained by setting `cl_var` when using `"robust"`/`"cluster"`, `"boot"`, or `"jack"`.
#' 
#' @examples
#' 
#' data(mroz)
#' mroz$incthou <- mroz$faminc / 1000
#' 
#' fit <- ml_lm(incthou ~ age + I(age^2) + huswage + educ + unem, 
#'              data = mroz)
#' 
#' # Different variance-covariance estimators
#' v_oim   <- vcov(fit, type = "oim")      # Observed Information Matrix (default)
#' v_opg   <- vcov(fit, type = "opg")      # Outer Product of Gradients (BHHH)
#' v_robust <- vcov(fit, type = "robust")   # Robust / Sandwich estimator
#' 
#' # Clustered robust standard errors
#' v_clust <- vcov(fit, type = "robust", cl_var = "age")
#' 
#' # Bootstrap variance-covariance matrix
#' v_boot  <- vcov(fit, type = "boot", repetitions = 100, seed = 123)
#' 
#' # Jackknife variance-covariance matrix
#' v_jack  <- vcov(fit, type = "jack")
#' 
#' # Compare standard errors across methods
#' sterrors <- data.frame(
#'   oim = sqrt(diag(v_oim)),
#'   opg = sqrt(diag(v_opg)),
#'   robust = sqrt(diag(v_robust)),
#'   cluster = sqrt(diag(v_clust)),
#'   bootstrap = sqrt(diag(v_boot)),
#'   jackknife = sqrt(diag(v_jack))
#' )
#' 
#' sterrors
#' 
#' @author Alfonso Sanchez-Penalver
#'
#' @export
vcov.mlmodel <- function(object,
                         type = "oim",
                         cl_var = NULL,
                         repetitions = 999,
                         seed = NULL,
                         progress = TRUE,
                         ...)
{
  # 1. Inheritance check - must be first
  if (!inherits(object, "mlmodel"))
    cli::cli_abort("`object` must be a model of class 'mlmodel'.", call = NULL)

  # 2. Validate and normalize type
  type <- rlang::arg_match(type, c("oim", "robust", "opg", "cluster", "boot", "jack", "jackknife"))

  if(type == "cluster" && is.null(cl_var))
    cli::cli_abort("`cl_var` cannot be null with 'cluster' `type`.",
                   call = NULL)

  # cluster is an alias for robust, since they must provide cl_var
  if (type == "cluster") type <- "robust"
  
  # jackknife is an alias for jack
  if (type %in% c("jack", "jackknife")) type <- "jack"

  # 3. Early validation for cl_var
  if (!is.null(cl_var) && !(type %in% c("robust", "boot", "jack")))
    cli::cli_abort(
      "`cl_var` can only be used when `type` is 'cluster', 'robust', 'boot', or 'jack'.",
      call = NULL
    )

  # 4. Process clustering variable if provided
  if (!is.null(cl_var)) {
    if (is.character(cl_var)) {
      # Retrieve data (new primary path first, then old d_name fallback)
      if (!is.null(object$model$data) && is.data.frame(object$model$data)) {
        d <- object$model$data
      } else if (!is.null(object$model$d_name) && object$model$d_name != "<unknown data>") {
        d <- tryCatch(get(object$model$d_name), error = function(e) {
          cli::cli_abort("Cannot retrieve the dataset to get the clustering variable.",
                         call = NULL)
        })
      } else {
        cli::cli_abort("Dataset and its name not stored; cannot retrieve clustering variable.",
                       call = NULL)
      }
      cl_var_name <- cl_var
      cl_var <- d[[cl_var]][object$model$sample]
    } else {
      # User passed a vector directly
      n_var  <- length(cl_var)
      n_orig <- object$model$n_orig
      n_used <- object$model$n_used
      if (n_var != n_orig && n_var != n_used)
        cli::cli_abort(
          c("The clustering vector has the wrong number of observations.",
            "It should have either {.val {n_orig}} (original) or {.val {n_used}} (used in estimation) observations."),
          call = NULL
        )
      if (n_var == n_orig)
        cl_var <- cl_var[object$model$sample]
      cl_var_name <- NULL
    }

    # Check for unusable observations in clustering variable
    if (any(!complete.cases(cl_var))) {
      n_bad <- sum(!complete.cases(cl_var))
      cli::cli_abort(
        c("The clustering variable has unusable observations (NA or NaN).",
          "Found {.val {n_bad}} bad observation{?s} in the estimation sample."),
        call = NULL
      )
    }
  }

  # -- 5. Now, we're ready to select the method.
  if (type == "boot")
  {
    vcov_mat <- .vcov_boot(object,
                           repetitions = repetitions,
                           seed = seed,
                           cl_var = cl_var,
                           progress = progress,
                           ...)
    attr(vcov_mat, "vcov.type") <- type
    if (!is.null(cl_var)) {
      attr(vcov_mat, "clustered") <- TRUE
      attr(vcov_mat, "cluster.var") <- cl_var
      attr(vcov_mat, "cluster.varname") <- cl_var_name
    }
    return(vcov_mat)
  }
  
  if (type == "jack")
  {
    vcov_mat <- .vcov_jack(object,
                           cl_var = cl_var,
                           progress = progress,
                           ...)
    attr(vcov_mat, "vcov.type") <- type
    if (!is.null(cl_var)) {
      attr(vcov_mat, "clustered") <- TRUE
      attr(vcov_mat, "cluster.var") <- cl_var
      attr(vcov_mat, "cluster.varname") <- cl_var_name
    }
    return(vcov_mat)
  }

  # Regular (non-bootstrap or jackknife) variance types
  H <- object$hessian
  if (is.null(H))
    cli::cli_abort("Hessian is missing from the model object.", call = NULL)

  # Eigen decomposition
  eig <- eigen(-H, symmetric = TRUE)
  eigvals <- eig$values

  # Singularity warning for OIM
  if (type == "oim") {
    n_neg <- sum(eigvals < 0)
    if (n_neg > 0) {
      cli::cli_abort(c("Hessian is not negative semidefinite.",
                       "i" = "It has {.val {n_neg}} positive eigenvalue{?s}."),
                     call = NULL)
    }

    # Relative and absolute singularity checks
    rel_sing <- min(eigvals) / max(eigvals) < 1e-12
    abs_sing <- which(abs(eigvals) < 1e-6)

    if (rel_sing || length(abs_sing) > 0) {
      if (is.null(object$.oim_singularity_warned)) {
        pnames <- names(coef(object))
        eigvecs <- eig$vectors

        cli::cli_alert_warning("OIM variance may be unreliable due to singularity in the Hessian.")

        if (rel_sing) {
          max_idx <- which.max(eigvals)
          min_idx <- which.min(eigvals)
          cli::cli_alert_info(
            "Relative singularity: {.val {pnames[max_idx]}} has the largest eigenvalue ({round(eigvals[max_idx], 3)}), \\
             while {.val {pnames[min_idx]}} has the smallest ({round(eigvals[min_idx], 6)})."
          )
        }

        if (length(abs_sing) > 0) {
          cli::cli_alert_info("Near-zero eigenvalues detected. The following linear combinations are nearly flat:")

          for (i in abs_sing) {
            par_dim <- pnames[i]
            vec <- eigvecs[, i]
            large_idx <- which(abs(vec) > 0.2)

            if (length(large_idx) > 0) {
              terms <- paste0(round(vec[large_idx], 3), " * ", pnames[large_idx])
              combo <- paste(terms, collapse = " + ")
              cli::cli_alert_info(
                "  {.val {par_dim}} (eigenvalue = {.val {round(eigvals[i], 6)}}): {.code {combo}}"
              )
            }
          }
        }

        cli::cli_alert_info("Consider using `type = 'robust'` or revising the model specification.")
      }
    }
  }

  V <- chol2inv(chol(-H))
  G <- object$gradientObs

  # Compute variance
  if (!is.null(cl_var)) {
    # Cluster-robust variance
    clusters <- unique(cl_var)
    n_cl <- length(clusters)
    S <- matrix(0, nrow = n_cl, ncol = ncol(G))
    for (i in seq_along(clusters)) {
      idx <- cl_var == clusters[i]
      S[i, ] <- colSums(G[idx, , drop = FALSE])
    }
    n <- n_cl
    vcov_mat <- V %*% crossprod(S) %*% V * n / (n - 1)
  } else if (type == "opg") {
    vcov_mat <- chol2inv(chol(crossprod(G)))
  } else if (type == "robust") {
    n <- nrow(G)
    vcov_mat <- V %*% crossprod(G) %*% V * n / (n - 1)
  } else {  # oim
    vcov_mat <- V
  }

  attr(vcov_mat, "vcov.type") <- type
  if (!is.null(cl_var)) {
    attr(vcov_mat, "clustered") <- TRUE
    attr(vcov_mat, "cluster.var") <- cl_var
    attr(vcov_mat, "cluster.varname") <- cl_var_name
  }
  dimnames(vcov_mat) <- list(names(coef(object)), names(coef(object)))
  return(vcov_mat)
}