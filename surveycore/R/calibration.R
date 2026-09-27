# R/calibration.R
#
# Calibration data constructor for survey design objects.
#
# Functions defined here:
#   as_caldata() — build a calibration data element for @calibration slot

# ── as_caldata ─────────────────────────────────────────────────────────────────

#' Build a Calibration Data Element
#'
#' Constructs a calibration data object from base sampling weights, g-weights
#' (calibration factors), and a model matrix of calibration covariates. The
#' returned list is suitable for assignment to the `@calibration` slot of a
#' [survey_taylor] or [survey_replicate] object.
#'
#' @param base_weights A numeric vector of positive, finite base sampling
#'   weights (length n). These are the original sampling weights before
#'   calibration is applied. Must not contain `NA`, `NaN`, or `Inf`.
#' @param g_weights A numeric vector of positive, finite g-factors (length n).
#'   The calibrated weights are `base_weights * g_weights`. g-factors equal
#'   to 1.0 represent no adjustment. Must not contain `NA`, `NaN`, or `Inf`.
#'   Must have the same length as `base_weights`. The internal quantity
#'   `g_weights * sqrt(base_weights)` must not have any near-zero values
#'   (below `sqrt(.Machine$double.eps)`); if it does, a
#'   `surveycore_error_caldata_weights_near_zero` warning is issued.
#' @param model_matrix A numeric matrix with `n` rows and at least 1 column,
#'   representing the calibration covariates used during post-stratification or
#'   raking. Must not contain `NA`, `NaN`, or `Inf`.
#'
#' @return A named list with four elements:
#'   \describe{
#'     \item{`qr`}{A QR decomposition (class `"qr"`) of
#'       `sqrt(base_weights) * model_matrix`. Used for calibration projection
#'       in variance estimation.}
#'     \item{`w`}{A numeric vector of length n equal to
#'       `g_weights * sqrt(base_weights)`. This intermediate quantity (the
#'       square root of the calibrated weights scaled by g) is used directly
#'       in the GREG variance projection formula.}
#'     \item{`stage`}{Integer scalar `0L`. Currently only between-PSU
#'       calibration (stage 0) is supported.}
#'     \item{`index`}{`NULL`. Reserved for future within-PSU calibration
#'       support.}
#'   }
#'
#' @details
#' The resulting calibration object is used by the variance estimation
#' routines to apply a Deville-Sarndal (1992) calibration correction to
#' Taylor-series and replicate-weight variance estimates.
#'
#' **GLM limitation**: Using a calibrated `survey_taylor` object with
#' [survey_glm()] produces correct but conservative standard errors until
#' GREG-GLM variance is implemented in a future release. The calibration
#' correction is not applied in the GLM variance path.
#'
#' @section Inter-package contract:
#' \strong{GREG calibration (single auxiliary variable or multiple uncorrelated
#' variables):} pass the model matrix from \code{model.matrix(formula, data)}
#' directly -- one column per calibration variable. The intercept column
#' (`(Intercept)`) is included by default from \code{model.matrix()}; it
#' contributes one degree of freedom to the calibration adjustment.
#'
#' \strong{Raking (multiple calibration margins, Architecture A):} combine all
#' margin indicator matrices into a single matrix before calling
#' \code{as_caldata()}. Column-bind the matrices and drop one reference column
#' per margin to avoid rank deficiency (e.g., drop the last column of each
#' per-margin block). Pass this single combined matrix. Do \strong{not} call
#' \code{as_caldata()} once per margin; that uses Architecture B (sequential),
#' which requires a separate \code{as_caldata()} element per calibration pass
#' and stores them all in the \code{@calibration} list.
#'
#' \strong{q_k = 1 assumption:} \code{as_caldata()} always assumes
#' \eqn{q_k = 1} (uniform calibration weights). If your calibration uses a
#' non-unity \eqn{q_k} (variance-function weights from \code{survey::calibrate(
#' calfun = "linear", variance = ...)}) you must absorb those weights into
#' \code{model_matrix} before calling \code{as_caldata()}.
#'
#' @section For \code{survey_replicate} designs:
#' \code{@calibration} on a \code{survey_replicate} object is
#' \strong{provenance-only}: it documents that the replicate weights were
#' derived from a calibrated design, but the variance estimator does not apply
#' any GREG projection. Calibration is already encoded in the replicate weights
#' themselves. Do not expect \code{get_means()} SE to differ between a
#' \code{survey_replicate} with and without \code{@calibration} set.
#'
#' @references
#' Deville, J.-C., and Sarndal, C.-E. (1992). Calibration estimators in
#' survey sampling. *Journal of the American Statistical Association*, 87,
#' 376–382.
#'
#' @examples
#' # Minimal example: 3-unit design, intercept-only calibration
#' base_weights <- c(2.5, 3.0, 4.0)
#' g_weights <- c(1.02, 0.98, 1.01)
#' model_matrix <- matrix(1, nrow = 3, ncol = 1)
#'
#' cd <- as_caldata(base_weights, g_weights, model_matrix)
#' names(cd) # "qr", "w", "stage", "index"
#'
#' # Assign to a survey_taylor design
#' df <- data.frame(y = c(1.2, 2.3, 3.4), wt = base_weights)
#' design <- as_survey(df, weights = wt)
#' design@calibration <- list(cd)
#' is.null(design@calibration) # FALSE
#' @seealso
#'   [as_survey()] for Taylor series designs,
#'   [as_survey_replicate()] for replicate-weight designs,
#'   [as_survey_twophase()] for two-phase designs
#' @family constructors
#' @export
as_caldata <- function(base_weights, g_weights, model_matrix) {
  # ── Validation: base_weights ─────────────────────────────────────────────────

  # Check 1: finite values first (NA/NaN/Inf)
  if (any(!is.finite(base_weights))) {
    n_bad <- sum(!is.finite(base_weights))
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg base_weights} contains non-finite values ",
          "({n_bad} value{?s})."
        ),
        "i" = paste0(
          "{.arg base_weights} must be a fully finite numeric vector. ",
          "Check for {.code NA}, {.code NaN}, or {.code Inf} values."
        ),
        "v" = "Remove or impute non-finite values before calling {.fn as_caldata}."
      ),
      class = "surveycore_error_caldata_weights_missing"
    )
  }

  # Check 2: length > 0 and all strictly positive
  if (length(base_weights) == 0L || any(base_weights <= 0)) {
    cli::cli_abort(
      c(
        "x" = "{.arg base_weights} must be strictly positive.",
        "i" = paste0(
          "All values must be > 0. Got {sum(base_weights <= 0, na.rm = TRUE)} ",
          "non-positive value{?s}."
        ),
        "v" = "Ensure all sampling weights are positive before calibration."
      ),
      class = "surveycore_error_caldata_weights_nonpositive"
    )
  }

  # ── Validation: g_weights ────────────────────────────────────────────────────

  # Check 3: finite values first (NA/NaN/Inf)
  if (any(!is.finite(g_weights))) {
    cli::cli_abort(
      c(
        "x" = "{.arg g_weights} contains non-finite values.",
        "i" = paste0(
          "{.arg g_weights} must be a fully finite numeric vector. ",
          "Check for {.code NA}, {.code NaN}, or {.code Inf} values."
        ),
        "v" = "Remove or impute non-finite values before calling {.fn as_caldata}."
      ),
      class = "surveycore_error_caldata_gweights_missing"
    )
  }

  # Check 4: length > 0 and all strictly positive
  if (length(g_weights) == 0L || any(g_weights <= 0)) {
    cli::cli_abort(
      c(
        "x" = "{.arg g_weights} must be strictly positive.",
        "i" = paste0(
          "All g-factors must be > 0. Got {sum(g_weights <= 0, na.rm = TRUE)} ",
          "non-positive value{?s}."
        ),
        "v" = "Ensure all g-factors are positive. A g-factor of 1.0 means no adjustment."
      ),
      class = "surveycore_error_caldata_gweights_nonpositive"
    )
  }

  # Check 5: length must match base_weights
  if (length(g_weights) != length(base_weights)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg g_weights} length ({length(g_weights)}) must equal ",
          "{.arg base_weights} length ({length(base_weights)})."
        ),
        "i" = paste0(
          "Each observation must have exactly one base weight and one g-factor. ",
          "Supply vectors of equal length."
        )
      ),
      class = "surveycore_error_caldata_gweights_length_mismatch"
    )
  }

  # Check 6: near-zero product check
  w_prod <- g_weights * sqrt(base_weights)
  if (any(w_prod < .Machine$double.eps^0.5)) {
    n_near_zero <- sum(w_prod < .Machine$double.eps^0.5)
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg g_weights} * sqrt({.arg base_weights}) contains ",
          "near-zero values ({n_near_zero} value{?s})."
        ),
        "i" = paste0(
          "The product {.arg g_weights} * sqrt({.arg base_weights}) must be ",
          "above {.code .Machine$double.eps^0.5} for all observations."
        ),
        "v" = "Review g-factors; values near 0 indicate near-zero calibrated weights."
      ),
      class = "surveycore_error_caldata_weights_near_zero"
    )
  }

  # ── Validation: model_matrix ─────────────────────────────────────────────────

  # Check 7: row count must match base_weights length
  if (nrow(model_matrix) != length(base_weights)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg model_matrix} has {nrow(model_matrix)} row{?s} but ",
          "{.arg base_weights} has length {length(base_weights)}."
        ),
        "i" = paste0(
          "The number of rows in {.arg model_matrix} must equal the number ",
          "of observations (length of {.arg base_weights})."
        )
      ),
      class = "surveycore_error_caldata_dimension_mismatch"
    )
  }

  # Check 8: must have at least 1 column
  if (ncol(model_matrix) == 0L) {
    cli::cli_abort(
      c(
        "x" = "{.arg model_matrix} must have at least 1 column.",
        "i" = paste0(
          "A calibration model requires at least one covariate column. ",
          "An intercept-only model uses a column of all 1s."
        ),
        "v" = paste0(
          "Use {.code matrix(1, nrow = n, ncol = 1)} for an intercept-only ",
          "calibration model."
        )
      ),
      class = "surveycore_error_caldata_empty_model_matrix"
    )
  }

  # Check 9: all finite (no NA/NaN/Inf)
  if (any(!is.finite(model_matrix))) {
    cli::cli_abort(
      c(
        "x" = "{.arg model_matrix} contains non-finite values.",
        "i" = paste0(
          "{.arg model_matrix} must contain only finite numeric values. ",
          "Check for {.code NA}, {.code NaN}, or {.code Inf} in the matrix."
        ),
        "v" = "Impute or remove non-finite entries before calling {.fn as_caldata}."
      ),
      class = "surveycore_error_caldata_model_matrix_invalid"
    )
  }

  # ── Build return value ───────────────────────────────────────────────────────
  list(
    qr = qr(sqrt(base_weights) * model_matrix),
    w = w_prod,
    stage = 0L,
    index = NULL
  )
}


# ── .validate_calibration_arg ─────────────────────────────────────────────────
#
# Validate the `calibration` constructor argument before the survey object is
# built. Called from as_survey() and as_survey_replicate() immediately before
# the S7 constructor call.
#
# @param calibration  The value supplied to the `calibration =` argument.
#   Must be NULL, an empty list, or a list of valid caldata objects.
# @param nrow_data    Integer. The number of rows in the design's data frame.
#   Each caldata element's `w` vector must have this length.
# @return Invisibly TRUE on success; aborts on failure.
# @noRd
.validate_calibration_arg <- function(calibration, nrow_data) {
  # Case 1: NULL is always valid (no calibration)
  if (is.null(calibration)) {
    return(invisible(TRUE))
  }

  # Case 2: empty list is valid (cleared calibration)
  if (is.list(calibration) && length(calibration) == 0L) {
    return(invisible(TRUE))
  }

  # Case 3: detect bare (un-wrapped) caldata -- a list whose top-level names are
  # exactly the four caldata keys. User passed as_caldata() result directly
  # instead of wrapping it in list().
  .caldata_keys <- c("qr", "w", "stage", "index")
  if (
    is.list(calibration) &&
      all(.caldata_keys %in% names(calibration))
  ) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg calibration} must be a list of {.fn as_caldata} outputs ",
          "or {.code NULL}."
        ),
        "i" = paste0(
          "Got {.cls {class(calibration)[[1L]]}}. It looks like you passed ",
          "the result of {.fn as_caldata} directly -- wrap it in ",
          "{.code list()}: {.code calibration = list(cd)}."
        )
      ),
      class = "surveycore_error_calibration_not_list"
    )
  }

  # Case 4: must be a plain list (data.frame is technically a list but not valid)
  if (!is.list(calibration) || is.data.frame(calibration)) {
    cli::cli_abort(
      c(
        "x" = paste0(
          "{.arg calibration} must be a list of {.fn as_caldata} outputs ",
          "or {.code NULL}."
        ),
        "i" = "Got {.cls {class(calibration)[[1L]]}}."
      ),
      class = "surveycore_error_calibration_not_list"
    )
  }

  # Case 5: loop over list elements -- each must be a valid caldata structure
  for (i in seq_along(calibration)) {
    cd <- calibration[[i]]
    is_valid <- (!is.null(cd) &&
      is.list(cd) &&
      all(c("qr", "w", "stage", "index") %in% names(cd)) &&
      length(cd$w) == nrow_data)
    if (!is_valid) {
      cli::cli_abort(
        c(
          "x" = paste0(
            "Element {.val {i}} of {.arg calibration} is not a valid ",
            "caldata object."
          ),
          "i" = paste0(
            "Each element must be a named list with fields {.val qr}, ",
            "{.val w}, {.val stage}, and {.val index}, produced by ",
            "{.fn as_caldata}."
          )
        ),
        class = "surveycore_error_caldata_invalid_element"
      )
    }
  }

  invisible(TRUE)
}


# ── .apply_caldata_projection ─────────────────────────────────────────────────
#
# Project the linearization matrix `u` onto the orthogonal complement of the
# calibration column space, following Deville & Sarndal (1992). This reduces
# variance estimates to account for the information in the calibration
# auxiliary variables.
#
# @param u        A numeric matrix (n × p). The linearized influence matrix.
# @param caldata  A list of caldata elements (each created by as_caldata()).
# @return The projected matrix `u`, same dimensions as input.
# @noRd
.apply_caldata_projection <- function(u, caldata) {
  # Guard 0: empty list pass-through
  if (length(caldata) == 0L) {
    return(u)
  }
  # Guard 1: all-NA pass-through
  if (all(is.na(u))) {
    return(u)
  }
  # Guard 1b: NULL element check
  bad_idx <- which(vapply(caldata, is.null, logical(1)))
  if (length(bad_idx) > 0L) {
    cli::cli_abort(
      c(
        "x" = "caldata element(s) {bad_idx} are NULL.",
        "i" = paste0(
          "All {.field @calibration} list elements must be constructed ",
          "via {.fn as_caldata}."
        ),
        "v" = "Inspect {.code design@calibration} for NULL entries."
      ),
      class = "surveycore_error_caldata_invalid_element"
    )
  }
  # Guard 2: stage check
  bad_stage <- vapply(caldata, function(cd) cd$stage != 0L, logical(1))
  if (any(bad_stage)) {
    n_bad <- sum(bad_stage)
    cli::cli_abort(
      c(
        "x" = "Within-PSU calibration (stage != 0) is not supported in v1.",
        "i" = paste0(
          "{n_bad} caldata element{?s} {?has/have} stage != 0L."
        ),
        "v" = "Use population-level calibration only."
      ),
      class = "surveycore_error_caldata_within_stage_unsupported"
    )
  }
  # Loop: project out each calibration's column space
  for (cd in caldata) {
    # Guard 3: dimension check
    if (nrow(u) != length(cd$w)) {
      cli::cli_abort(
        c(
          "x" = "Calibration projection dimension mismatch.",
          "i" = paste0(
            "Linearization vector has {nrow(u)} rows; ",
            "caldata {.code w} has length {length(cd$w)}."
          )
        ),
        class = "surveycore_error_caldata_projection_dimension_mismatch"
      )
    }
    u <- qr.resid(cd$qr, u / cd$w) * cd$w
  }
  u
}


# ── .maybe_apply_calibration ──────────────────────────────────────────────────
#
# Apply calibration projection if the design has calibration data, otherwise
# return the linearized matrix unchanged.
#
# @param linearized  A numeric matrix. The linearized influence matrix.
# @param design      A survey_taylor or survey_replicate object.
# @return The (possibly projected) linearized matrix.
# @noRd
.maybe_apply_calibration <- function(linearized, design) {
  if (!is.null(design@calibration) && length(design@calibration) > 0L) {
    .apply_caldata_projection(linearized, design@calibration)
  } else {
    linearized
  }
}


# ── .get_calibration_df_reduction ─────────────────────────────────────────────
#
# Compute the total degrees-of-freedom reduction from calibration. This equals
# the sum of QR ranks across all caldata elements in the design's @calibration
# list.
#
# @param design  A survey_taylor or survey_replicate object.
# @return Integer(1). 0L when no calibration is set.
# @noRd
.get_calibration_df_reduction <- function(design) {
  if (!is.null(design@calibration) && length(design@calibration) > 0L) {
    sum(vapply(design@calibration, function(cd) cd$qr$rank, integer(1)))
  } else {
    0L
  }
}
