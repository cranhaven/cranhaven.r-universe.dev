# CODEBOOK AND DATA DICTIONARY GENERATION
# Constructs detailed variable-level data dictionaries documenting column types,
# labels, measurement units, categories, missingness, and distribution summaries.

#########
# CODEBOOK GENERATOR DISPATCH
# S3 method dispatch across raw data frames, specifications, results, and composed reports.

#' Build a SimtablR codebook
#'
#' @param x A data frame, `simtab_spec`, `simtab_result`, or `simtab_report`.
#' @param ... Ignored.
#' @return A `simtab_codebook` data frame with one row per variable.
#' @details For reports, a publication label recorded consistently by one or
#'   more items is used. If report items record conflicting labels for the same
#'   variable, `codebook()` warns and uses the neutral label stored on the
#'   source column rather than choosing an arbitrary item by position.
#' @examples
#' cb <- codebook(epitabl[, c("age", "sex", "diabetes")])
#' head(cb)
#' @export
codebook <- function(x, ...) {
  UseMethod("codebook")
}

#' @export
codebook.data.frame <- function(x, ...) {
  .new_simtab_codebook(.codebook_frame(x))
}

#' @export
codebook.simtab_spec <- function(x, ...) {
  x <- validate_simtab_spec(x)
  .new_simtab_codebook(
    .codebook_frame(x$data_src$ref$data, labels = .codebook_labels(x))
  )
}

#' @export
codebook.simtab_result <- function(x, ...) {
  x <- validate_simtab_result(x)
  .new_simtab_codebook(.codebook_frame(x$used$ref$data, labels = .codebook_labels(x)))
}

#' @export
codebook.simtab_report <- function(x, ...) {
  x <- validate_simtab_report(x)
  .new_simtab_codebook(
    .codebook_frame(x$used$ref$data, labels = .report_codebook_labels(x))
  )
}

#' @export
codebook.default <- function(x, ...) {
  simtab_abort_spec(c(
    "{.fn codebook} requires a data frame or SimtablR object.",
    "i" = "Pass a data.frame, simtab_spec, simtab_result, or simtab_report."
  ))
}

#' Construct a simtab_codebook data frame subclass
#' @keywords internal
#' @noRd
.new_simtab_codebook <- function(df) {
  class(df) <- c("simtab_codebook", "data.frame")
  df
}

#' @export
print.simtab_codebook <- function(x, ...) {
  cat("<simtab_codebook>\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}

#' Convert a SimtablR codebook to a flextable
#'
#' @param x A `simtab_codebook` object.
#' @param ... Additional arguments passed to [flextable::flextable()].
#' @return A styled `flextable` object.
#' @examples
#' if (requireNamespace("flextable", quietly = TRUE)) {
#'   cb <- codebook(epitabl[, c("age", "sex", "diabetes")])
#'   flextable::as_flextable(cb)
#' }
#' @exportS3Method flextable::as_flextable
as_flextable.simtab_codebook <- function(x, ...) {
  .require_pkg("flextable")
  simtab_theme(flextable::flextable(as.data.frame(x), ...))
}

#########
# LABEL RECONCILIATION AND OVERRIDE EXTRACTION
# Harmonizes variable labels across specification layers and detects report label conflicts.

#' Extract user-defined or metadata variable labels from specification or result
#' @keywords internal
#' @noRd
.codebook_labels <- function(x) {
  if (inherits(x, "simtab_spec")) {
    return(x$fmt$labels %||% NULL)
  }

  labels <- x$meta$labels %||% x$spec$fmt$labels %||% NULL
  predictor_labels <- x$meta$predictor_labels %||% NULL
  if (is.null(labels)) {
    return(predictor_labels)
  }
  if (is.null(predictor_labels)) {
    return(labels)
  }
  c(labels, predictor_labels)
}

#' Reconcile publication labels recorded across multi-table report items
#'
#' A label shared by all items that record the variable is unambiguous. When
#' item labels conflict, the variable is omitted from the override vector so
#' `.codebook_label()` falls back to the source-column label. The warning keeps
#' the conflict visible without inventing positional precedence.
#' @keywords internal
#' @noRd
.report_codebook_labels <- function(x) {
  recorded <- lapply(x$items, .codebook_labels)
  recorded <- do.call(c, unname(recorded))
  if (length(recorded) == 0) {
    return(NULL)
  }

  keep <- !is.na(recorded) & nzchar(recorded) &
    !is.na(names(recorded)) & nzchar(names(recorded))
  recorded <- recorded[keep]
  if (length(recorded) == 0) {
    return(NULL)
  }

  by_variable <- split(unname(recorded), names(recorded))
  unique_labels <- lapply(by_variable, unique)
  conflicts <- names(unique_labels)[lengths(unique_labels) > 1L]
  if (length(conflicts) > 0) {
    cli::cli_warn(
      c(
        "Report items contain conflicting codebook labels.",
        "i" = "Source-data labels will be used for: {paste(conflicts, collapse = ', ')}.",
        "v" = "Use one publication label per variable across report items."
      ),
      class = "simtab_warning_codebook"
    )
  }

  resolved <- unique_labels[lengths(unique_labels) == 1L]
  if (length(resolved) == 0) {
    return(NULL)
  }
  stats::setNames(vapply(resolved, `[[`, character(1), 1L), names(resolved))
}

#########
# COLUMN PROFILE AND DATA DICTIONARY BUILDERS
# Evaluates variable types, distinct counts, factor levels, missingness, and summary statistics.

#' Build rectangular data frame of column profiles across dataset
#' @keywords internal
#' @noRd
.codebook_frame <- function(data, labels = NULL) {
  if (!is.data.frame(data)) {
    simtab_abort_spec("'data' must be a data.frame.")
  }
  if (ncol(data) == 0L) {
    return(data.frame(
      variable = character(),
      label = character(),
      type = character(),
      n_unique = integer(),
      `levels/units` = character(),
      `n_missing (%)` = character(),
      summary = character(),
      check.names = FALSE
    ))
  }

  rows <- lapply(seq_along(data), function(i) {
    var <- names(data)[[i]]
    x <- data[[i]]
    n_unique <- length(unique(x[!is.na(x)]))
    label <- .codebook_label(x, var, labels)
    levels_units <- .codebook_levels_units(x, n_unique = n_unique)
    data.frame(
      variable = var,
      label = label,
      type = .codebook_type(x),
      n_unique = as.integer(n_unique),
      `levels/units` = levels_units,
      `n_missing (%)` = .codebook_missing(x),
      summary = .codebook_summary(x, n_unique = n_unique),
      check.names = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Resolve display label for a single variable with precedence overrides
#' @keywords internal
#' @noRd
.codebook_label <- function(x, var, labels = NULL) {
  if (!is.null(labels) && var %in% names(labels) &&
      is.character(labels[[var]]) && length(labels[[var]]) == 1 &&
      !is.na(labels[[var]]) && nzchar(labels[[var]])) {
    return(unname(labels[[var]]))
  }
  label <- attr(x, "label", exact = TRUE)
  if (is.character(label) && length(label) == 1 && !is.na(label) && nzchar(label)) {
    return(label)
  }
  var
}

#' Classify variable into SimtablR statistical data type category
#' @keywords internal
#' @noRd
.codebook_type <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return("date/time")
  }
  if (is.logical(x)) {
    return("logical")
  }
  if (is.factor(x) || is.numeric(x) || is.integer(x) || is.character(x)) {
    return(.detect_var_type(x))
  }
  class(x)[1]
}

#' Extract measurement units or representative factor/character levels
#' @keywords internal
#' @noRd
.codebook_levels_units <- function(x, n_unique = length(unique(x[!is.na(x)]))) {
  units <- attr(x, "units", exact = TRUE)
  if (is.character(units) && length(units) == 1 && nzchar(units)) {
    return(units)
  }
  if (is.factor(x) && n_unique <= 10L) {
    return(paste(levels(x), collapse = ", "))
  }
  if (is.logical(x)) {
    return("FALSE, TRUE")
  }
  if (is.character(x) && n_unique <= 10L) {
    vals <- unique(x[!is.na(x)])
    vals <- vals[order(vals)]
    return(paste(utils::head(vals, 10), collapse = ", "))
  }
  ""
}

#' Format count and percentage of missing observations
#' @keywords internal
#' @noRd
.codebook_missing <- function(x) {
  n <- length(x)
  missing <- sum(is.na(x))
  pct <- if (n == 0) 0 else 100 * missing / n
  sprintf("%d (%.1f%%)", missing, pct)
}

#' Format concise descriptive summary string for column values
#' @keywords internal
#' @noRd
.codebook_summary <- function(x, n_unique = length(unique(x[!is.na(x)]))) {
  y <- x[!is.na(x)]
  if (length(y) == 0) {
    return("all missing")
  }
  if (is.numeric(y) || is.integer(y)) {
    return(sprintf(
      "mean %.2f; median %.2f; range %.2f to %.2f",
      mean(y),
      stats::median(y),
      min(y),
      max(y)
    ))
  }
  if (inherits(y, c("Date", "POSIXct", "POSIXlt"))) {
    return(sprintf("range %s to %s", min(y), max(y)))
  }
  if (is.character(y) && n_unique > 10L) {
    return(sprintf("%d unique values", n_unique))
  }
  vals <- if (is.factor(y)) levels(y) else unique(as.character(y))
  vals <- vals[nzchar(vals)]
  paste(utils::head(vals, 10), collapse = ", ")
}
