# GENERIC PRESENTATION AND EXTRACTION DISPATCH
# Routes print, data frame coercion, tidy, glance, flextable, and journal styling
# through engine vtable contracts without mutating underlying evidence.

#########
# RENDERER UTILITIES AND SPEC EVALUATION
# Helper routines for footer styling, evaluation guards, and console print arguments.

#' Append formatted footnote lines to flextable footer
#' @keywords internal
#' @noRd
.flex_add_footnotes <- function(ft, footnotes = NULL) {
  if (length(footnotes) == 0) {
    return(ft)
  }
  for (note in footnotes) {
    ft <- flextable::add_footer_lines(ft, values = as.character(note))
  }
  flextable::align(ft, part = "footer", align = "left")
}

#' Evaluate specification into result if object is uncomputed
#' @keywords internal
#' @noRd
.compute_if_spec <- function(x) {
  if (inherits(x, "simtab_spec")) {
    return(evaluate(x))
  }
  x
}

#' Validate logical details flag for console print methods
#' @keywords internal
#' @noRd
.validate_print_details <- function(details) {
  if (!is.logical(details) || length(details) != 1L || is.na(details)) {
    simtab_abort_input(c(
      "{.arg details} must be {.code TRUE} or {.code FALSE}.",
      "i" = "Received: {.val {details}}.",
      "v" = "Use {.code print(x, details = TRUE)} for the expanded header."
    ))
  }
  details
}

#########
# S3 PRESENTATION GENERICS AND DISPATCH
# Dispatches print, as.data.frame, tidy, glance, and as_flextable to registered engine renderers.

#' Print a computed SimtablR result
#'
#' Dispatches to the engine's registered `print` renderer. Falls back to an
#' honest generic (header + `as_data_frame` renderer if available + advice)
#' when the engine registered no `print` renderer.
#'
#' @param x A `simtab_result`.
#' @param ... Passed to subclass renderers. For `table1` and `regtab`,
#'   `details = TRUE` restores the decorative result summary and stored call.
#'   For `regtab`, `vif = TRUE` adds VIF-equivalent columns to the display frame.
#' @return Invisibly returns `x`.
#' @examples
#' res <- tb(epitabl, sex, diabetes)
#' print(res)
#' @export
print.simtab_result <- function(x, ...) {
  x <- validate_simtab_result(x)
  fn <- .engine_renderer(x, "print")
  if (is.null(fn)) {
    cat("<simtab_result>\n")
    cat("  computed evidence object\n")
    if (!is.null(x$meta$engine)) {
      cat("  engine: ", x$meta$engine, "\n", sep = "")
    }
    df_fn <- .engine_renderer(x, "as_data_frame")
    if (!is.null(df_fn)) {
      print(df_fn(x))
    }
    .print_advice(x)
    return(invisible(x))
  }
  fn(x, ...)
}

#' Convert a SimtablR result or spec to a data frame
#'
#' @param x A `simtab_result` or `simtab_spec`.
#' @param row.names Unused.
#' @param optional Unused.
#' @param tidy Logical. `FALSE` returns the formatted display frame; `TRUE`
#'   returns the long numeric table.
#' @param ... Passed to subclass renderers. For `regtab`, `vif = TRUE` adds
#'   maximum VIF-equivalent columns by outcome.
#' @return A data.frame.
#' @examples
#' res <- tb(epitabl, sex, diabetes)
#' as.data.frame(res)
#' as.data.frame(res, tidy = TRUE)
#' @export
as.data.frame.simtab_result <- function(x, row.names = NULL, optional = FALSE, tidy = FALSE, ...) {
  x <- validate_simtab_result(x)
  fn <- .engine_renderer(x, "as_data_frame")
  if (is.null(fn)) {
    engine <- x$meta$engine %||% "?"
    simtab_abort_render(c(
      sprintf("No {.fn as.data.frame} renderer is registered for engine {.val %s}.", engine),
      "i" = "The result exists, but SimtablR does not know how to tabulate this engine.",
      "v" = "Register one with {.code register_engine(..., renderers = list(as_data_frame = function(x, ...) ...))}."
    ))
  }
  fn(x, row.names = row.names, optional = optional, tidy = tidy, ...)
}

#' @rdname as.data.frame.simtab_result
#' @export
as.data.frame.simtab_spec <- function(x, row.names = NULL, optional = FALSE, tidy = FALSE, ...) {
  as.data.frame(evaluate(x), row.names = row.names, optional = optional, tidy = tidy, ...)
}

#' Tidy a SimtablR result
#'
#' @param x A `simtab_result` or `simtab_spec`.
#' @param ... Ignored.
#' @return A long data.frame. For `table1`, this matches
#'   `as.data.frame(x, tidy = TRUE)`.
#' @examples
#' res <- tb(epitabl, sex, diabetes)
#' generics::tidy(res)
#' @export
tidy.simtab_result <- function(x, ...) {
  x <- validate_simtab_result(x)
  as.data.frame(x, tidy = TRUE)
}

#' @rdname tidy.simtab_result
#' @export
tidy.simtab_spec <- function(x, ...) {
  generics::tidy(evaluate(x), ...)
}

#' Glance a SimtablR result
#'
#' @param x A `simtab_result` or `simtab_spec`.
#' @param ... Ignored.
#' @return A one-row summary data.frame, where available.
#' @examples
#' fit <- regtab(epitabl, "adjudicated_acs", ~ age + sex, family = binomial())
#' generics::glance(fit)
#' @export
glance.simtab_result <- function(x, ...) {
  x <- validate_simtab_result(x)
  fn <- .engine_renderer(x, "glance")
  if (is.null(fn)) {
    engine <- x$meta$engine %||% "?"
    simtab_abort_render(c(
      sprintf("No {.fn glance} renderer is registered for engine {.val %s}.", engine),
      "i" = "The result exists, but no one-row summary renderer is available for this engine.",
      "v" = "Register one with {.code register_engine(..., renderers = list(glance = function(x, ...) ...))}."
    ))
  }
  fn(x, ...)
}

#' @rdname glance.simtab_result
#' @export
glance.simtab_spec <- function(x, ...) {
  generics::glance(evaluate(x), ...)
}

#' Convert a SimtablR result or spec to a flextable
#'
#' @param x A `simtab_result` or `simtab_spec`.
#' @param ... Passed to the subclass flextable renderer.
#' @return A `flextable` object, or a named list of `flextable` objects for a
#'   `simtab_report`.
#' @examples
#' if (requireNamespace("flextable", quietly = TRUE)) {
#'   res <- tb(epitabl, sex, diabetes)
#'   flextable::as_flextable(res)
#' }
#' @export
as_flextable.simtab_result <- function(x, ...) {
  x <- validate_simtab_result(x)
  fn <- .engine_renderer(x, "as_flextable")
  if (is.null(fn)) {
    engine <- x$meta$engine %||% "?"
    simtab_abort_render(c(
      sprintf("No {.fn as_flextable} renderer is registered for engine {.val %s}.", engine),
      "i" = "The result exists, but no Word/PowerPoint table renderer is available for this engine.",
      "v" = "Register one with {.code register_engine(..., renderers = list(as_flextable = function(x, ...) ...))}."
    ))
  }
  fn(x, ...)
}

#' @rdname as_flextable.simtab_result
#' @export
as_flextable.simtab_spec <- function(x, ...) {
  .require_pkg("flextable")
  flextable::as_flextable(evaluate(x), ...)
}

#########
# RESULT RESTYLING DISPATCH
# Updates stored presentation journal style on a computed result without recalculation.

#' @export
style.simtab_result <- function(x, journal) {
  x <- validate_simtab_result(x)
  if (missing(journal)) {
    simtab_abort_input(c(
      "{.arg journal} is required.",
      "i" = "{.fn style} needs to know which presentation to apply.",
      "v" = "Call {.code style(x, \"lancet\")}, or see {.fn list_journals}."
    ))
  }

  # Validate that the requested style is resolvable, but keep the raw value so
  # per-variable style lists still work when supplied through table constructors.
  .resolve_table_style(journal)
  x$spec$style <- journal
  x$meta$style <- journal
  if (!is.null(x$meta$args)) {
    x$meta$args$style <- journal
  }
  x
}

#' @export
style.simtab_report <- function(x, journal) {
  x <- validate_simtab_report(x)
  if (missing(journal)) {
    simtab_abort_input(c(
      "{.arg journal} is required.",
      "i" = "{.fn style} needs to know which presentation to apply.",
      "v" = "Call {.code style(report, \"lancet\")}, or see {.fn list_journals}."
    ))
  }
  x$items <- lapply(x$items, style, journal = journal)
  x
}

#' @export
style.simtab_rbind_tb <- function(x, journal) {
  if (missing(journal)) {
    simtab_abort_input(c(
      "{.arg journal} is required.",
      "i" = "{.fn style} needs to know which presentation to apply.",
      "v" = "Call {.code style(stacked, \"lancet\")}, or see {.fn list_journals}."
    ))
  }
  # Stacked tables keep each component's metadata; the bivariate renderer reads
  # the journal from there, so restyle every component without touching $data.
  .resolve_table_style(journal)
  x$meta$tables <- lapply(x$meta$tables, function(meta) {
    meta$style <- journal
    meta
  })
  x
}
