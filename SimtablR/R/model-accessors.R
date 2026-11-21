# STATISTICAL MODEL EVIDENCE ACCESSORS
# Implements standard S3 statistical methods (coef, confint, vcov, formula, nobs)
# and model_info diagnostics over retained regression and survival model evidence.

#' Access retained model evidence
#'
#' `regtab()` and `survtab()` are reporting-evidence objects rather than mutable
#' fitted-model objects. These methods expose the conventional model evidence
#' that SimtablR retains: link-scale coefficients, confidence intervals,
#' formulas, analysed observation counts, and covariance matrices.
#'
#' A single-outcome result returns the conventional vector, matrix, formula, or
#' scalar. A multi-outcome result returns a list named by outcome, except
#' [stats::nobs()] which returns a named integer vector. Supply `outcome` to
#' select one outcome. Unknown, non-scalar, and failed-outcome selections raise
#' `simtab_error_model`.
#'
#' Confidence intervals replay the inference represented by the result. Wald
#' intervals can be returned at another `level` from copied coefficient and
#' covariance evidence. Firth profile intervals are retained only at the level
#' used to fit the result; requesting another level requires refitting and is
#' therefore rejected.
#'
#' SimtablR deliberately does not retain fitted values, residuals, response
#' vectors, mutable fitted-model objects, or a prediction/forecasting surface.
#' Use [stats::glm()], `survival::coxph()`, or
#' `logistf::logistf()` directly when those model-object workflows are required.
#'
#' @param object,x A computed `regtab()` or `survtab()` result.
#' @param outcome Optional single outcome name. Omit it for the conventional
#'   single-outcome value or a predictably named multi-outcome collection.
#' @param parm Optional coefficient names or positions for [stats::confint()].
#' @param level Confidence level for [stats::confint()].
#' @param complete Retained for compatibility with [stats::vcov()]. SimtablR
#'   returns the complete copied covariance matrix.
#' @param ... Unused.
#' @return `coef()` returns a named numeric vector or named list of vectors;
#'   `confint()` and `vcov()` return a matrix or named list of matrices;
#'   `formula()` returns a formula or named list of formulas; `nobs()` returns
#'   an integer scalar or named integer vector.
#' @examples
#' data(epitabl)
#' fit <- regtab(
#'   epitabl,
#'   outcomes = "rehospitalized",
#'   predictors = ~ age + sex,
#'   family = binomial("logit"),
#'   robust = FALSE
#' )
#' coef(fit)
#' confint(fit)
#' formula(fit)
#' nobs(fit)
#' vcov(fit)
#' @importFrom stats coef confint formula nobs vcov
#' @name model-accessors
NULL

#########
# MODEL EVIDENCE DISPATCH AND EXTRACTION
# Resolves outcome-specific evidence blocks and maps accessors across outcomes.

#' Extract retained model evidence dictionary for specified outcome
#' @keywords internal
#' @noRd
.model_evidence <- function(object, outcome = NULL, accessor = "model accessor") {
  object <- validate_simtab_result(object)
  evidence <- object$meta$model_evidence
  if (!is.list(evidence) || length(evidence) == 0L || is.null(names(evidence))) {
    simtab_abort_model(c(
      "{accessor} is not available for this SimtablR result.",
      "i" = "The {.val {object$meta$engine %||% class(object)[[1L]]}} engine does not retain conventional model evidence.",
      "v" = "Use the result's tidy, glance, or engine-specific accessors instead."
    ))
  }

  if (is.null(outcome)) {
    return(evidence)
  }
  if (!is.character(outcome) || length(outcome) != 1L || is.na(outcome) || !nzchar(outcome)) {
    simtab_abort_model(c(
      "{.arg outcome} must be one non-empty outcome name.",
      "i" = "Available fitted outcomes: {.val {names(evidence)}}.",
      "v" = "Select one outcome by name."
    ))
  }
  if (!outcome %in% names(evidence)) {
    known <- object$meta$model_info$outcome %||% object$meta$outcomes %||% names(evidence)
    if (outcome %in% known) {
      simtab_abort_model(c(
        "No model evidence is available for outcome {.val {outcome}}.",
        "i" = "That outcome failed during model fitting.",
        "v" = "Inspect {.code model_info(result, outcome = {outcome})} for the recorded failure."
      ))
    }
    simtab_abort_model(c(
      "Unknown model outcome {.val {outcome}}.",
      "i" = "Available fitted outcomes: {.val {names(evidence)}}.",
      "v" = "Select one of the listed outcomes."
    ))
  }
  evidence[outcome]
}

#' Map extractor function over outcome model evidence
#' @keywords internal
#' @noRd
.map_model_evidence <- function(object, outcome, accessor, function_) {
  evidence <- .model_evidence(object, outcome = outcome, accessor = accessor)
  values <- lapply(evidence, function_)
  if (!is.null(outcome) || length(values) == 1L) {
    return(values[[1L]])
  }
  values
}

#########
# STANDARD STATISTICAL EXTRACTOR S3 METHODS
# Implements coef, vcov, formula, nobs, and confint for SimtablR model results.

#' @rdname model-accessors
#' @export
coef.simtab_result <- function(object, outcome = NULL, ...) {
  .map_model_evidence(
    object, outcome, "coef()",
    function(evidence) evidence$coefficients
  )
}

#' @rdname model-accessors
#' @export
vcov.simtab_result <- function(object, outcome = NULL, complete = TRUE, ...) {
  .map_model_evidence(
    object, outcome, "vcov()",
    function(evidence) evidence$covariance
  )
}

#' @rdname model-accessors
#' @export
formula.simtab_result <- function(x, outcome = NULL, ...) {
  .map_model_evidence(
    x, outcome, "formula()",
    function(evidence) evidence$formula
  )
}

#' @rdname model-accessors
#' @export
nobs.simtab_result <- function(object, outcome = NULL, ...) {
  evidence <- .model_evidence(object, outcome = outcome, accessor = "nobs()")
  values <- vapply(evidence, function(item) as.integer(item$n), integer(1))
  if (!is.null(outcome) || length(values) == 1L) {
    return(unname(values[[1L]]))
  }
  values
}

#' Compute or subset confidence intervals from retained model evidence
#' @keywords internal
#' @noRd
.model_confint <- function(evidence, parm, level) {
  if (!is.numeric(level) || length(level) != 1L || is.na(level) || level <= 0 || level >= 1) {
    simtab_abort_model(c(
      "{.arg level} must be one probability strictly between 0 and 1.",
      "i" = "Received {.val {level}}.",
      "v" = "Use a value such as {.code 0.95}."
    ))
  }

  if (identical(evidence$interval_method, "profile")) {
    if (!isTRUE(all.equal(level, evidence$conf.level, tolerance = .Machine$double.eps^0.5))) {
      simtab_abort_model(c(
        "A different confidence level is not available for this retained profile interval.",
        "i" = "The result stores a {format(100 * evidence$conf.level)}% profile interval.",
        "v" = "Refit {.fn regtab} with the required {.arg conf.level}."
      ))
    }
    interval <- evidence$interval
  } else {
    critical <- stats::qnorm(1 - (1 - level) / 2)
    standard_error <- sqrt(diag(evidence$covariance))
    interval <- cbind(
      evidence$coefficients - critical * standard_error,
      evidence$coefficients + critical * standard_error
    )
    alpha <- (1 - level) / 2
    colnames(interval) <- paste0(
      format(100 * c(alpha, 1 - alpha), trim = TRUE, scientific = FALSE),
      " %"
    )
  }

  if (!is.null(parm)) {
    valid <- if (is.character(parm)) {
      length(parm) > 0L && all(parm %in% rownames(interval))
    } else if (is.numeric(parm)) {
      length(parm) > 0L && all(is.finite(parm)) &&
        all(parm == as.integer(parm)) && all(parm >= 1L & parm <= nrow(interval))
    } else {
      FALSE
    }
    if (!valid) {
      simtab_abort_model(c(
        "{.arg parm} contains unknown coefficient names or positions.",
        "i" = "Available coefficients: {.val {rownames(interval)}}.",
        "v" = "Select retained coefficients by name or position."
      ))
    }
    interval <- interval[parm, , drop = FALSE]
  }
  interval
}

#' @rdname model-accessors
#' @export
confint.simtab_result <- function(object, parm, level = 0.95, outcome = NULL, ...) {
  selected_parm <- if (missing(parm)) NULL else parm
  .map_model_evidence(
    object, outcome, "confint()",
    function(evidence) .model_confint(evidence, selected_parm, level)
  )
}

#########
# MODEL CONVERGENCE AND ESTIMATOR DIAGNOSTICS
# Exposes fit status, optimizer convergence, and boundary condition diagnostics.

#' Inspect retained model convergence information
#'
#' Returns the stable model-information rows retained by `regtab()` or
#' `survtab()`, including analysed N, estimator, convergence, boundary, failure,
#' and error fields where applicable. Unlike the conventional accessors,
#' `model_info()` may select a failed outcome because its purpose is to inspect
#' that failure.
#'
#' @param object A computed `regtab()` or `survtab()` result.
#' @param outcome Optional single outcome name.
#' @param ... Unused.
#' @return A data frame with one row per requested model.
#' @examples
#' data(epitabl)
#' fit <- regtab(
#'   epitabl, "rehospitalized", ~ age + sex,
#'   family = binomial("logit"), robust = FALSE
#' )
#' model_info(fit)
#' @export
model_info <- function(object, outcome = NULL, ...) {
  UseMethod("model_info")
}

#' @export
model_info.simtab_result <- function(object, outcome = NULL, ...) {
  object <- validate_simtab_result(object)
  information <- object$meta$model_info
  if (!is.data.frame(information) || !"outcome" %in% names(information)) {
    simtab_abort_model(c(
      "model_info() is not available for this SimtablR result.",
      "i" = "The {.val {object$meta$engine %||% class(object)[[1L]]}} engine is not a fitted regression model.",
      "v" = "Use the result's tidy, glance, or engine-specific accessors instead."
    ))
  }
  if (is.null(outcome)) {
    return(information)
  }
  if (!is.character(outcome) || length(outcome) != 1L || is.na(outcome) ||
      !nzchar(outcome) || !outcome %in% information$outcome) {
    simtab_abort_model(c(
      "Unknown or ambiguous model outcome selection.",
      "i" = "Available outcomes: {.val {information$outcome}}.",
      "v" = "Select one outcome by name."
    ))
  }
  information[information$outcome == outcome, , drop = FALSE]
}

#' @export
model_info.default <- function(object, outcome = NULL, ...) {
  simtab_abort_model(c(
    "{.fn model_info} requires a SimtablR fitted-model result.",
    "i" = "Received an object of class {.cls {class(object)[[1L]]}}.",
    "v" = "Pass a result from {.fn regtab} or {.fn survtab}."
  ))
}
