#' Parameter estimates from lavaan with a unified schema
#'
#' Extracts raw or standardized coefficients from a fitted lavaan model,
#' always ensuring a `group` column is present (set to 1 for single-group models).
#' Internally, the function relies on `lavaan::parameterEstimates()` for raw
#' estimates and `lavaan::standardizedSolution()` for standardized coefficients.
#'
#' @details
#' This wrapper harmonizes the output structure between raw and standardized
#' estimates, renames standardized columns to a unified schema (e.g., `est`
#' instead of `est.std`), and ensures that a `group` column is always included.
#' When `include_r2 = TRUE`, R² values are appended for each endogenous variable
#' if available in the model.
#'
#' @param fit A fitted `lavaan` object.
#' @param level Confidence level for intervals (default `0.95`).
#' @param standardized Either a logical (FALSE/TRUE) or one of
#'   `c("none", "std.all", "std.lv", "std.nox")`. If `TRUE`, it is treated as `"std.all"`.
#' @param include_r2 Logical; include R-squared rows (only when `standardized = "none"`),
#'   default `TRUE`.
#'
#' @return A `data.frame` in the style of `lavaan::parameterEstimates()`. If
#'   standardized output is requested, the estimate is in column `est`
#'   (renamed from `est.std` for consistency).
#'
#' @examples
#' HS.model <- ' visual  =~ x1 + x2 + x3
#'               textual =~ x4 + x5 + x6
#'               speed   =~ x7 + x8 + x9 '
#' fit <- lavaan::cfa(HS.model,
#'                    data = lavaan::HolzingerSwineford1939)
#'
#' # Raw estimates with R2
#' pe  <- parameter_estimates(fit)
#'
#' # Standardized (std.all)
#' pes <- parameter_estimates(fit, standardized = TRUE)
#'
#' # Standardized (std.lv)
#' pes_lv <- parameter_estimates(fit, standardized = "std.lv")
#'
#' @importFrom lavaan parameterEstimates standardizedSolution
#' @export
parameter_estimates <- function(fit,
                                level = 0.95,
                                standardized = "none",
                                include_r2 = TRUE) {
  # Basic assertion (internal or replace with your own guard)
  .assert_lavaan_fit(fit)

  # Normalize 'standardized' argument
  if (is.logical(standardized)) {
    std_type <- if (isTRUE(standardized)) "std.all" else "none"
  } else {
    std_type <- match.arg(standardized, c("none", "std.all", "std.lv", "std.nox"))
  }

  # Extract estimates
  if (identical(std_type, "none")) {
    out <- lavaan::parameterEstimates(fit, level = level, rsquare = isTRUE(include_r2))
  } else {
    out <- lavaan::standardizedSolution(fit, level = level, type = std_type)
    # Unify the estimate column name to 'est'
    if ("est.std" %in% names(out)) {
      names(out)[names(out) == "est.std"] <- "est"
    }
  }

  # Ensure 'group' column exists and is integer-coded
  if (!("group" %in% names(out))) {
    out$group <- NA_integer_
  }
  # If single-group, set group to 1L
  ng <- tryCatch(lavaan::lavInspect(fit, "ngroups"), error = function(e) NA_integer_)
  if (isTRUE(ng == 1L)) {
    out$group[] <- 1L
  } else {
    # Make integer where possible; leave NAs if truly not applicable
    suppressWarnings(out$group <- as.integer(out$group))
  }

  # Return tibble
  tibble::as_tibble(out)
}
