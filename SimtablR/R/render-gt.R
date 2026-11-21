# GT HTML AND QUARTO TABLE RENDERERS
# Formats display data frames into gt HTML table objects with stratification spanners,
# source notes, and statistical summaries for interactive and document reporting.

#########
# S3 GENERICS AND METHOD DISPATCH
# S3 method dispatch for converting specifications and results into gt tables.

#' Convert a SimtablR object to a gt table
#'
#' `as_gt()` is a soft-dependency renderer for HTML and Quarto workflows. It
#' auto-computes a `simtab_spec`, then dispatches by result subclass. Built-in
#' results with specialised renderers retain their display features; every other
#' result with an `as.data.frame` renderer is converted from its display data
#' frame. Extension engines can register a custom `as_gt` renderer.
#'
#' @param x A SimtablR result or spec.
#' @param ... Passed to `gt::gt()`.
#' @return A `gt_tbl` object, or a named list of `gt_tbl` objects for a
#'   `simtab_report`.
#' @examples
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   res <- tb(epitabl, sex, diabetes)
#'   as_gt(res)
#' }
#' @export
as_gt <- function(x, ...) {
  UseMethod("as_gt")
}

#' @rdname as_gt
#' @export
as_gt.simtab_spec <- function(x, ...) {
  as_gt(evaluate(x), ...)
}

#' @rdname as_gt
#' @export
as_gt.simtab_result <- function(x, ...) {
  x <- validate_simtab_result(x)
  fn <- .engine_renderer(x, "as_gt")
  if (!is.null(fn)) {
    return(fn(x, ...))
  }
  if (inherits(x, "simtab_diag")) {
    return(.diag_as_gt(x, ...))
  }
  if (inherits(x, "simtab_tb")) {
    return(.tb_as_gt(x, ...))
  }
  if (inherits(x, "simtab_table1")) {
    return(.table1_as_gt(x, ...))
  }
  if (inherits(x, "simtab_regtab")) {
    return(.regtab_as_gt(x, ...))
  }
  .gt_from_display(as.data.frame(x, tidy = FALSE), ...)
}

#' @rdname as_gt
#' @export
as_gt.simtab_rbind_tb <- function(x, ...) {
  .gt_from_display(as.data.frame(x, tidy = FALSE), ...)
}

#########
# GT BUILDER UTILITIES AND ENGINE SPECIALIZATIONS
# Constructs gt tables with source notes, spanners, and confusion matrix footnotes.

#' Construct a gt table from display data frame with optional source notes
#' @keywords internal
#' @noRd
.gt_from_display <- function(df, ..., footnotes = NULL) {
  .require_export_pkg("gt")
  attrs <- attributes(df)
  attr(df, "row_type") <- NULL
  attr(df, "row_var") <- NULL
  attr(df, "row_level") <- NULL
  tab <- gt::gt(df, ...)
  attr(tab, "simtab_display_attrs") <- attrs
  if (length(footnotes) > 0) {
    for (note in footnotes) {
      tab <- gt::tab_source_note(tab, source_note = note)
    }
  }
  tab
}

#' Render Table 1 descriptive result as a gt table with column spanners
#' @keywords internal
#' @noRd
.table1_as_gt <- function(x, footnotes = NULL, ...) {
  df <- as.data.frame(x, tidy = FALSE)
  tab <- .gt_from_display(df, ..., footnotes = c(.table1_notes(x), footnotes))
  spanner <- .table1_strat_spanner(x, df)
  if (!is.null(spanner)) {
    cols <- names(df)[spanner$columns]
    tab <- gt::tab_spanner(
      tab,
      label = spanner$label,
      columns = tidyselect::all_of(cols)
    )
  }
  tab
}

#' Render tb bivariate result as a gt table with statistical test footnotes
#' @keywords internal
#' @noRd
.tb_as_gt <- function(x, ...) {
  foot <- character(0)
  stats <- x$meta$stats
  if (!is.null(stats)) {
    p_str <- sub("^p ", "", .fmt_tb_p(
      stats$p.value, x$meta$decimal_mark %||% ".", .result_journal(x$meta$style)
    ))
    foot <- paste0(stats$method, ": p-value ", p_str)
  }
  .gt_from_display(as.data.frame(x, tidy = FALSE), ..., footnotes = foot)
}

#' Render diagnostic test result as a gt table with confusion matrix footnotes
#' @keywords internal
#' @noRd
.diag_as_gt <- function(x, ...) {
  x <- .diag_validate_result(x)
  tab <- .gt_from_display(as.data.frame(x, tidy = FALSE), ...)
  cm <- x$data$confusion_matrix
  gt::tab_source_note(
    tab,
    source_note = sprintf(
      "Confusion matrix: TP=%d, FP=%d, FN=%d, TN=%d.",
      cm[1, 1], cm[1, 2], cm[2, 1], cm[2, 2]
    )
  )
}

#' Render regression result as a gt table with failed outcome notes
#' @keywords internal
#' @noRd
.regtab_as_gt <- function(x, ...) {
  tab <- .gt_from_display(as.data.frame(x, tidy = FALSE), ...)
  if (isTRUE(x$meta$n_failed > 0)) {
    failed <- x$meta$model_info[x$meta$model_info$failed, "outcome", drop = TRUE]
    tab <- gt::tab_source_note(tab, source_note = paste0("Failed outcomes: ", paste(failed, collapse = ", ")))
  }
  tab
}
