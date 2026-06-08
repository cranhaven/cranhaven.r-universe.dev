# R/decompose.R — decompose_uncertainty(): variance decomposition
# Reports a three-way split (parameter / environmental / residual) for iid
# models, and a four-way split adding a temporal-autocorrelation component
# when the model formula contains an ar() / ma() / arma() / cosy() / unstr()
# / sar() / car() term.

#' Extract or recompute uncertainty decomposition
#'
#' Returns a \code{data.frame} with the uncertainty decomposition stored
#' inside an \code{et_prediction} object:
#' \describe{
#'   \item{param_var}{Variance of the posterior linear predictor — captures
#'     uncertainty in fitted regression coefficients.}
#'   \item{env_var}{Additional variance arising from measurement or
#'     prediction uncertainty in the predictor values (estimated via
#'     perturbation in \code{\link{et_predict}}).  Zero when
#'     \code{env_noise = NULL}.}
#'   \item{residual_var}{Posterior mean of \eqn{\sigma^2} (or its
#'     family-specific analogue) — biological process noise, unmeasured
#'     drivers, and drift. For autocorrelation models this is the
#'     \emph{innovation} variance, not the stationary marginal variance;
#'     the autocorrelated accumulation is reported separately in
#'     \code{temporal_var}.}
#'   \item{temporal_var}{(Only present when the model formula contains an
#'     autocorrelation term such as \code{ar()}, \code{ma()},
#'     \code{arma()}, \code{cosy()}, \code{unstr()}, \code{sar()}, or
#'     \code{car()}.) Variance attributable to residual temporal or
#'     spatial dependence beyond the iid \code{param + residual} sum,
#'     computed as
#'     \code{pmax(0, total_var - (param_var + residual_var))}.
#'     \code{env_var} is deliberately excluded from this gap because it
#'     is an additive perturbation-based augmentation measured outside
#'     of \code{posterior_predict}.}
#'   \item{total_var}{Variance of the full posterior predictive draws,
#'     including any autocorrelation structure modelled by brms.}
#' }
#'
#' All variance components are guaranteed non-negative. When
#' \code{temporal_var} is present, \code{param_var + residual_var +
#' temporal_var} reconstructs \code{total_var} (modulo Monte Carlo error);
#' when it is absent, \code{param_var + residual_var} does.
#' \code{env_var} is always additive on top, representing the extra
#' variance that would be contributed by perturbing predictors with
#' \code{env_noise}.
#'
#' @param predictions An \code{et_prediction} object from
#'   \code{\link{et_predict}}, or an \code{et_prediction_list} (grouped).
#' @param ... Unused.
#' @return A \code{data.frame} with columns
#'   \code{obs_id, param_var, env_var, residual_var, total_var}
#'   (plus \code{temporal_var} for autocorrelation models, and a leading
#'   \code{group} column for grouped predictions).
#' @examples
#' \donttest{
#' set.seed(1)
#' df  <- data.frame(y = rnorm(20), x1 = rnorm(20))
#' fit <- et_fit(y ~ x1, data = df,
#'               chains = 1, iter = 500, warmup = 250,
#'               cores = 1, refresh = 0)
#' new_df <- data.frame(x1 = rnorm(5))
#' pred   <- et_predict(fit, newdata = new_df,
#'                      env_noise = list(x1 = 0.2),
#'                      n_draws = 200, n_perturb = 50)
#' decomp <- decompose_uncertainty(pred)
#' head(decomp)
#' }
#' @export
decompose_uncertainty <- function(predictions, ...) {
  UseMethod("decompose_uncertainty")
}

#' @export
decompose_uncertainty.et_prediction <- function(predictions, ...) {
  predictions$decomposition
}

#' @export
decompose_uncertainty.et_prediction_list <- function(predictions, ...) {
  parts <- lapply(names(predictions$predictions), function(g) {
    pred <- predictions$predictions[[g]]
    if (is.null(pred)) return(NULL)
    d <- pred$decomposition
    cbind(data.frame(group = g, stringsAsFactors = FALSE), d)
  })
  do.call(rbind, Filter(Negate(is.null), parts))
}

#' @export
decompose_uncertainty.default <- function(predictions, ...) {
  stop("decompose_uncertainty() expects an et_prediction or et_prediction_list object.")
}
