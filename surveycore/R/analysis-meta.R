# R/analysis-meta.R
#
# The meta() generic for accessing structured metadata from survey result
# objects, and the print method for survey_result.

# ── meta() generic ────────────────────────────────────────────────────────────

#' Extract Metadata from a Survey Result
#'
#' Retrieves the structured metadata list attached to a survey result object
#' returned by any `get_*()` analysis function.
#'
#' This is the **only** supported way to access result metadata — do not use
#' `attr(result, ".meta")` directly.
#'
#' @param x A `survey_result` object returned by any `get_*()` function.
#' @param ... Currently unused. Reserved for future extensions.
#'
#' @return A named list. Common fields present on every result:
#' \describe{
#'   \item{`design_type`}{Character(1). Design class: `"taylor"`,
#'     `"replicate"`, `"twophase"`, or `"nonprob"`. SRS designs are
#'     represented as `survey_taylor` (no IDs/strata) and report
#'     `"taylor"`.}
#'   \item{`conf_level`}{Numeric(1). Confidence level used (e.g. `0.95`).}
#'   \item{`call`}{Language. Matched call to the `get_*()` function.}
#'   \item{`n_respondents`}{Integer(1). Total rows in the design, regardless
#'     of groups, domain status, or weights.}
#'   \item{`group`}{Named list. One entry per grouping variable; empty list
#'     (`list()`) when no groups are active. Each entry is a named list with:
#'     `variable_label` (character or `NULL`), `question_preface` (character
#'     or `NULL`), `value_labels` (named vector or `NULL`).}
#'   \item{`x`}{Named list. One entry per focal variable. Length 1 for
#'     single-x functions (`get_means`, `get_totals`, `get_quantiles`);
#'     length N for multi-x functions (`get_freqs`, `get_corr`). Each entry
#'     has the same sub-structure as `group` entries. `NULL` for
#'     `get_totals()` when called without an `x` argument.}
#' }
#' Function-specific additional fields:
#' \describe{
#'   \item{`probs`}{(`get_quantiles` only) Numeric vector of quantile
#'     probabilities.}
#'   \item{`method`}{(`get_corr` only) Character(1) correlation method.}
#'   \item{`numerator`, `denominator`}{(`get_ratios` only) Flat named lists
#'     with keys `name`, `variable_label`, `question_preface`, `value_labels`.}
#' }
#'
#' @examples
#' # Construct a minimal survey_result to illustrate meta():
#' result <- structure(
#'   tibble::tibble(mean = 42.0, se = 1.5, n = 100L),
#'   .meta = list(
#'     design_type = "taylor",
#'     conf_level = 0.95,
#'     call = quote(get_means(d, x)),
#'     n_respondents = 100L,
#'     group = list(),
#'     x = list(
#'       x = list(
#'         variable_label = NULL,
#'         question_preface = NULL,
#'         value_labels = NULL
#'       )
#'     )
#'   ),
#'   class = c("survey_means", "survey_result", "tbl_df", "tbl", "data.frame")
#' )
#' meta(result)$design_type # "taylor"
#' meta(result)$n_respondents # 100L
#' meta(result)$conf_level # 0.95
#' @family analysis
#' @export
meta <- function(x, ...) UseMethod("meta")

#' @rdname meta
#' @export
meta.survey_result <- function(x, ...) attr(x, ".meta")


# ── print.survey_result ───────────────────────────────────────────────────────

#' Print a Survey Result Object
#'
#' Prints a labelled header showing the specific result class and dimensions,
#' then delegates to the tibble print method for the tabular content.
#'
#' @param x A `survey_result` object.
#' @param ... Passed to the tibble print method.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' result <- structure(
#'   tibble::tibble(mean = 42.0, se = 1.5, n = 100L),
#'   .meta = list(
#'     design_type = "taylor",
#'     conf_level = 0.95,
#'     call = quote(get_means(d, x)),
#'     n_respondents = 100L,
#'     group = list(),
#'     x = list(
#'       x = list(
#'         variable_label = NULL,
#'         question_preface = NULL,
#'         value_labels = NULL
#'       )
#'     )
#'   ),
#'   class = c("survey_means", "survey_result", "tbl_df", "tbl", "data.frame")
#' )
#' print(result)
#' @method print survey_result
#' @export
print.survey_result <- function(x, ...) {
  cls <- class(x)[1L]
  dims <- paste(nrow(x), "\u00d7", ncol(x))
  cat(sprintf("# A <%s> [%s]\n", cls, dims))
  NextMethod()
  invisible(x)
}


# ── print.survey_diffs ──────────────────────────────────────────────────────

#' Print a Survey Diffs Result
#'
#' Prints a structured header showing design type, family, dependent
#' variable, treatment variable with reference level, and estimation
#' method, then delegates to the tibble print method for the body.
#'
#' @param x A `survey_diffs` object.
#' @param ... Passed to the tibble print method.
#'
#' @return `x`, invisibly.
#'
#' @method print survey_diffs
#' @export
print.survey_diffs <- function(x, ...) {
  m <- attr(x, ".meta")

  design_display <- switch(
    m$design_type,
    taylor = "Taylor series",
    replicate = "Replicate weights",
    twophase = "Two-phase",
    nonprob = "Non-probability",
    m$design_type
  )

  x_name <- names(m$x)[1L]
  treats_name <- m$treats$name
  ref_level <- m$treats$ref_level

  cat("# A survey_diffs result\n")
  cat(sprintf(
    "# Design: %s | Family: %s (%s)\n",
    design_display,
    m$family,
    m$link
  ))
  cat(sprintf(
    "# DV: %s | Treatment: %s (ref: %s)\n",
    x_name,
    treats_name,
    ref_level
  ))
  cat(sprintf(
    "# Method: %s / %s\n",
    m$estimate_method,
    m$mean_method
  ))
  NextMethod()
  invisible(x)
}
