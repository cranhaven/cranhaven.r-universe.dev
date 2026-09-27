# R/glm-marginaleffects.R
#
# marginaleffects extension interface for survey_glm_fit.
#
# Implements the four S3 generics that marginaleffects dispatches through,
# enabling avg_slopes(), avg_predictions(), and the full marginaleffects API
# on survey_glm_fit objects.
#
# Spec reference: plans/spec-phase-2.md §VII
# Registration: R/zzz.R (.onLoad, conditional on marginaleffects installed)
#
# None of the four methods are exported. All are registered dynamically via
# registerS3method() in .onLoad(). Use #' @noRd to suppress .Rd generation.

# ---------------------------------------------------------------------------
# 7.3  get_coef.survey_glm_fit
# ---------------------------------------------------------------------------

#' @noRd
get_coef.survey_glm_fit <- function(model, ...) {
  model@coefficients
}

# ---------------------------------------------------------------------------
# 7.4  set_coef.survey_glm_fit
# ---------------------------------------------------------------------------
#
# Returns a modified copy with both @coefficients (S7 property) and
# fit_$coefficients (internal stats::glm object) updated.
#
# Why both must be patched:
#   predict.survey_glm_fit() delegates to stats::predict.glm(object@fit_).
#   stats::predict.glm() reads coefficients from fit$coefficients, not from
#   the S7 @coefficients property. Without patching fit_, predictions during
#   marginaleffects' numerical gradient evaluation would ignore the perturbed
#   coefficients and every AME would silently equal zero.

#' @noRd
set_coef.survey_glm_fit <- function(model, coefs, ...) {
  model@coefficients <- coefs
  if (!is.null(model@fit_)) {
    model@fit_$coefficients <- coefs
  }
  model
}

# ---------------------------------------------------------------------------
# 7.5  get_vcov.survey_glm_fit
# ---------------------------------------------------------------------------
#
# Returns the design-based variance-covariance matrix (Binder 1983 sandwich).
# AME standard errors produced by avg_slopes() are a hybrid: the variance of
# β̂ is design-based (from @vcov), while the Jacobian is computed numerically
# at a single point estimate via the delta method. This is methodologically
# valid — equivalent to what survey::svyglm + marginaleffects produces.

#' @noRd
get_vcov.survey_glm_fit <- function(model, ...) {
  model@vcov
}

# ---------------------------------------------------------------------------
# 7.6  get_predict.survey_glm_fit
# ---------------------------------------------------------------------------
#
# Returns predictions as a data frame with columns rowid (integer) and
# estimate (numeric). marginaleffects always operates on the response scale.
#
# The argument is named `newdata` (not `new_data`) to match the marginaleffects
# extension interface contract. predict.survey_glm_fit() uses `new_data`,
# so the call maps new_data = newdata explicitly.
#
# Requires model@fit_ to be non-NULL. If fit_ is NULL (e.g. after
# deserialization), errors with surveycore_error_predict_no_fit — the same
# error class as predict.survey_glm_fit().

#' @noRd
get_predict.survey_glm_fit <- function(model, newdata, ...) {
  pred <- stats::predict(model, new_data = newdata, type = "response")
  data.frame(
    rowid = seq_len(nrow(newdata)),
    estimate = as.numeric(pred)
  )
}
