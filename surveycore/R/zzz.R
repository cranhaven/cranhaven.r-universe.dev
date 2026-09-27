# R/zzz.R
#
# Package load hook. S7 requires methods_register() to be called in .onLoad()
# so that methods on external generics (print, summary, etc.) are registered
# correctly when the package is loaded from an installed library.
# See: vignette("packages", package = "S7")
#
# Phase 2 additions: register all 22 S3 methods + getCall for survey_glm_fit.
# CRITICAL: S7 namespaced class names ("surveycore::survey_glm_fit") must be
# used as the class string — not bare "survey_glm_fit". The class() of an S7
# object returns namespaced names; bare names never match.
# See: vignette("packages", package = "S7") for background on .onLoad() method
# registration for S7 objects.

# nocov start
.onLoad <- function(libname, pkgname) {
  S7::methods_register()

  # Phase 2: survey_glm_fit S3 methods
  registerS3method(
    "print",
    "surveycore::survey_glm_fit",
    print.survey_glm_fit,
    envir = asNamespace("base")
  )
  registerS3method(
    "summary",
    "surveycore::survey_glm_fit",
    summary.survey_glm_fit,
    envir = asNamespace("base")
  )
  registerS3method(
    "coef",
    "surveycore::survey_glm_fit",
    coef.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "vcov",
    "surveycore::survey_glm_fit",
    vcov.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "predict",
    "surveycore::survey_glm_fit",
    predict.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "fitted",
    "surveycore::survey_glm_fit",
    fitted.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "residuals",
    "surveycore::survey_glm_fit",
    residuals.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "confint",
    "surveycore::survey_glm_fit",
    confint.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "formula",
    "surveycore::survey_glm_fit",
    formula.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "terms",
    "surveycore::survey_glm_fit",
    terms.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "model.matrix",
    "surveycore::survey_glm_fit",
    model.matrix.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "model.frame",
    "surveycore::survey_glm_fit",
    model.frame.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "deviance",
    "surveycore::survey_glm_fit",
    deviance.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "df.residual",
    "surveycore::survey_glm_fit",
    df.residual.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "nobs",
    "surveycore::survey_glm_fit",
    nobs.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "hatvalues",
    "surveycore::survey_glm_fit",
    hatvalues.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "logLik",
    "surveycore::survey_glm_fit",
    logLik.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "AIC",
    "surveycore::survey_glm_fit",
    AIC.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "BIC",
    "surveycore::survey_glm_fit",
    BIC.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "update",
    "surveycore::survey_glm_fit",
    update.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "anova",
    "surveycore::survey_glm_fit",
    anova.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "getCall",
    "surveycore::survey_glm_fit",
    getCall.survey_glm_fit,
    envir = asNamespace("stats")
  )
  registerS3method(
    "plot",
    "surveycore::survey_glm_fit",
    plot.survey_glm_fit,
    envir = asNamespace("graphics")
  )

  # Phase 2: broom compatibility (conditional)
  if (requireNamespace("broom", quietly = TRUE)) {
    registerS3method(
      "tidy",
      "surveycore::survey_glm_fit",
      tidy.survey_glm_fit,
      envir = asNamespace("broom")
    )
    registerS3method(
      "tidy",
      "survey_variance",
      tidy.survey_variance,
      envir = asNamespace("broom")
    )
    registerS3method(
      "glance",
      "survey_variance",
      glance.survey_variance,
      envir = asNamespace("broom")
    )
    registerS3method(
      "tidy",
      "survey_covariance",
      tidy.survey_covariance,
      envir = asNamespace("broom")
    )
    registerS3method(
      "glance",
      "survey_covariance",
      glance.survey_covariance,
      envir = asNamespace("broom")
    )
  }

  # Phase 2: marginaleffects extension (conditional)
  if (requireNamespace("marginaleffects", quietly = TRUE)) {
    registerS3method(
      "get_coef",
      "surveycore::survey_glm_fit",
      get_coef.survey_glm_fit,
      envir = asNamespace("marginaleffects")
    )
    registerS3method(
      "set_coef",
      "surveycore::survey_glm_fit",
      set_coef.survey_glm_fit,
      envir = asNamespace("marginaleffects")
    )
    registerS3method(
      "get_vcov",
      "surveycore::survey_glm_fit",
      get_vcov.survey_glm_fit,
      envir = asNamespace("marginaleffects")
    )
    registerS3method(
      "get_predict",
      "surveycore::survey_glm_fit",
      get_predict.survey_glm_fit,
      envir = asNamespace("marginaleffects")
    )
    # Register class in marginaleffects supported-class list.
    # marginaleffects ≥ 0.25 requires classes to be listed in
    # getOption("marginaleffects_model_classes") in addition to implementing
    # the extension interface methods. Without this, avg_slopes() /
    # avg_predictions() reject the model with "class not supported".
    # Existing entries are preserved.
    existing <- getOption("marginaleffects_model_classes", default = NULL)
    if (!"surveycore::survey_glm_fit" %in% existing) {
      options(
        marginaleffects_model_classes = c(
          existing,
          "surveycore::survey_glm_fit"
        )
      )
    }
  }
}
# nocov end
