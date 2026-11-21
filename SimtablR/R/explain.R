# CONVERSATIONAL DECISION EXPLANATION AND AUDIT TRAIL
# Formats narrative explanations of recorded analytical decisions, design resolutions,
# missingness policies, and active advisory notices across specifications and results.

#########
# EXPLANATION GENERATOR DISPATCH
# S3 method dispatch across uncomputed specifications, computed results, and composed reports.

#' Report why SimtablR made each recorded decision
#'
#' `why()` prints the decisions already recorded on a spec, result, or
#' report. Passing a `simtab_spec` is inert: it describes the planned analysis
#' without computing it.
#'
#' @param x A `simtab_spec`, `simtab_result`, or `simtab_report`.
#' @param ... Ignored.
#' @return A `simtab_explanation` object, invisibly when printed.
#' @examples
#' res <- tb(epitabl, sex, diabetes)
#' why(res)
#' @export
why <- function(x, ...) {
  UseMethod("why")
}

#' @export
why.simtab_spec <- function(x, ...) {
  x <- validate_simtab_spec(x)
  lines <- c(
    "SimtablR explanation",
    "Status: not yet computed; this spec is an inert description.",
    sprintf("Engine: %s", x$engine %||% "not resolved"),
    sprintf("Design: %s", x$design %||% "not recorded"),
    sprintf("Measure: %s", x$effect$measure %||% "not selected"),
    sprintf("Measure source: %s", x$effect$resolved_from %||% "not recorded"),
    sprintf("Missingness: display=%s, denominator=%s, model=%s", x$missing$display, x$missing$denominator, x$missing$model_na),
    sprintf("Ruleset version: %s", .simtab_ruleset_version())
  )
  .new_simtab_explanation(lines, subject_class = class(x))
}

#' @export
why.simtab_result <- function(x, ...) {
  x <- validate_simtab_result(x)
  advice <- .filter_advice_for_guidance(x$advice %||% list(), simtablr_guidance())
  lines <- c(
    "SimtablR explanation",
    "Status: computed result.",
    sprintf("Engine: %s", x$meta$engine %||% x$spec$engine %||% "not recorded"),
    sprintf("Design: %s", .result_design(x) %||% "not recorded"),
    sprintf("Measure: %s", x$spec$effect$measure %||% x$meta$effect %||% "not selected"),
    sprintf("Measure source: %s", x$spec$effect$resolved_from %||% "not recorded"),
    sprintf("Ruleset version: %s", x$meta$ruleset_version %||% .simtab_ruleset_version())
  )
  if (length(advice) > 0) {
    lines <- c(lines, "Advice:", vapply(advice, .format_advice_line, character(1), level = simtablr_guidance()))
  }
  .new_simtab_explanation(lines, subject_class = class(x))
}

#' @export
why.simtab_report <- function(x, ...) {
  x <- validate_simtab_report(x)
  advice <- .filter_advice_for_guidance(x$advice %||% list(), simtablr_guidance())
  lines <- c(
    "SimtablR explanation",
    sprintf("Status: report with %d item(s).", length(x$items)),
    sprintf("Methods: %s", x$methods %||% "not recorded"),
    sprintf("Ruleset version: %s", .simtab_ruleset_version())
  )
  if (length(advice) > 0) {
    lines <- c(lines, "Advice:", vapply(advice, .format_advice_line, character(1), level = simtablr_guidance()))
  }
  .new_simtab_explanation(lines, subject_class = class(x))
}

#########
# EXPLANATION OBJECT CONSTRUCTION AND DISPLAY
# Builds simtab_explanation structures and formats multi-line console output.

#' Construct and print a simtab_explanation container object
#' @keywords internal
#' @noRd
.new_simtab_explanation <- function(lines, subject_class) {
  out <- structure(
    list(lines = lines, subject_class = subject_class),
    class = c("simtab_explanation", "simtab")
  )
  print(out)
  invisible(out)
}

#' @export
print.simtab_explanation <- function(x, ...) {
  cat(paste(x$lines, collapse = "\n"), "\n", sep = "")
  invisible(x)
}
