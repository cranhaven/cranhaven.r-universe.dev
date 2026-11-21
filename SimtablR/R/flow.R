# PARTICIPANT FLOW AND ATTRITION ACCOUNTING
# Compiles sample attrition, subsetting, rendered table sizes, and complete-case
# model counts from analysis metadata for STROBE flow reporting.

#########
# FLOW GENERATOR DISPATCH
# Assembles flow records from result and report metadata objects.

#' Build a participant flow table from recorded facts
#'
#' `flow()` assembles source, subset, rendered-cohort, complete-case, and model
#' Ns from a result or report without fitting or recomputing anything.
#'
#' @param x A `simtab_result` or `simtab_report`.
#' @param ... Ignored.
#' @return A `simtab_flow` object.
#' @examples
#' res <- tb(epitabl, sex, diabetes)
#' flow(res)
#' @export
flow <- function(x, ...) {
  UseMethod("flow")
}

#' @export
flow.simtab_result <- function(x, ...) {
  x <- validate_simtab_result(x)
  .new_simtab_flow(.flow_rows_for_items(list(result = x), used = x$used))
}

#' @export
flow.simtab_report <- function(x, ...) {
  x <- validate_simtab_report(x)
  .new_simtab_flow(.flow_rows_for_items(x$items, used = x$used))
}

#' Construct a simtab_flow container object
#' @keywords internal
#' @noRd
.new_simtab_flow <- function(rows) {
  structure(list(data = rows), class = c("simtab_flow", "simtab"))
}

#' Assemble participant flow rows across constituent report items
#' @keywords internal
#' @noRd
.flow_rows_for_items <- function(items, used) {
  source_n <- as.integer(used$nrow %||% items[[1]]$spec$data_src$nrow %||% NA_integer_)
  rows <- list(.flow_row("Unknown before SimtablR", NA_integer_, NA_integer_, "Rows excluded before data reached SimtablR are not recorded."))
  rows[[length(rows) + 1L]] <- .flow_row("Source data", source_n, NA_integer_, "Rows captured by SimtablR.")

  for (nm in names(items)) {
    rows <- c(rows, .flow_item_rows(items[[nm]], nm, source_n))
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Extract participant attrition rows for a single analysis item
#' @keywords internal
#' @noRd
.flow_item_rows <- function(item, name, source_n) {
  rendered_n <- .flow_rendered_n(item)
  rows <- list()
  subset_label <- .flow_subset_label(item)
  if (!is.null(subset_label)) {
    rows[[length(rows) + 1L]] <- .flow_row(
      sprintf("%s subset", name),
      rendered_n,
      .flow_excluded(source_n, rendered_n),
      subset_label
    )
  }

  if (inherits(item, "simtab_table1")) {
    rows[[length(rows) + 1L]] <- .flow_row(
      sprintf("%s source/header cohort", name),
      rendered_n,
      .flow_excluded(source_n, rendered_n),
      "Source N shown in the table header; grouped columns can exclude records with a missing stratification value."
    )
  }

  complete_model_n <- .flow_complete_model_n(item)
  if (!is.na(complete_model_n) && !is.na(rendered_n) && complete_model_n < rendered_n) {
    rows[[length(rows) + 1L]] <- .flow_row(
      sprintf("%s complete/model cases", name),
      complete_model_n,
      rendered_n - complete_model_n,
      "Complete-case/model N recorded in result metadata."
    )
  }

  rows
}

#########
# FLOW ROW CALCULATORS AND METADATA EXTRACTION
# Resolves stage sample sizes, exclusions, and subsetting annotations.

#' Construct a single participant flow data frame record
#' @keywords internal
#' @noRd
.flow_row <- function(stage, n, excluded, reason) {
  data.frame(
    stage = stage,
    N = as.integer(n),
    excluded = as.integer(excluded),
    reason = reason
  )
}

#' Compute sample count difference between successive stages
#' @keywords internal
#' @noRd
.flow_excluded <- function(from, to) {
  if (is.na(from) || is.na(to)) {
    return(NA_integer_)
  }
  as.integer(from - to)
}

#' Extract effective rendered sample size from item metadata
#' @keywords internal
#' @noRd
.flow_rendered_n <- function(item) {
  if (inherits(item, "simtab_table1")) {
    return(as.integer(item$meta$n_total %||% item$used$nrow %||% NA_integer_))
  }
  if (inherits(item, "simtab_tb") && !is.null(item$data$frequencies)) {
    return(as.integer(sum(item$data$frequencies, na.rm = TRUE)))
  }
  as.integer(item$used$nrow %||% item$spec$data_src$nrow %||% NA_integer_)
}

#' Extract complete-case or fitted model sample size from item metadata
#' @keywords internal
#' @noRd
.flow_complete_model_n <- function(item) {
  diag_n <- if (inherits(item, "simtab_diag")) sum(item$data$confusion_matrix, na.rm = TRUE) else NA_integer_
  roc_n <- if (inherits(item, "simtab_roc")) min(item$meta$sample_sizes$n, na.rm = TRUE) else NA_integer_
  cox_n <- if (inherits(item, "simtab_cox")) item$meta$n %||% NA_integer_ else NA_integer_
  vals <- c(
    item$meta$complete_case_n %||% NA_integer_,
    item$meta$model_n %||% NA_integer_,
    diag_n,
    roc_n,
    cox_n
  )
  vals <- as.integer(vals[!is.na(vals)])
  if (length(vals) == 0) {
    return(NA_integer_)
  }
  min(vals)
}

#' Retrieve formatted expression string describing subsetting criteria
#' @keywords internal
#' @noRd
.flow_subset_label <- function(item) {
  label <- item$spec$layout$subset_expr %||% NULL
  if (is.character(label) && length(label) == 1 && nzchar(label)) {
    return(label)
  }
  subset_expr <- item$spec$engine_opts$bivariate$subset %||% NULL
  if (is.null(subset_expr)) {
    return(NULL)
  }
  paste(deparse(subset_expr), collapse = " ")
}

#########
# S3 PRESENTATION AND RENDERING METHODS
# Formats participant flow tables for data frame, console, flextable, and ggplot2 autoplot.

#' @export
as.data.frame.simtab_flow <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$data
}

#' @export
print.simtab_flow <- function(x, ...) {
  cat("<simtab_flow>\n")
  print(as.data.frame(x), row.names = FALSE)
  invisible(x)
}

#' @rdname flow
#' @param x A `simtab_flow` object.
#' @exportS3Method flextable::as_flextable
as_flextable.simtab_flow <- function(x, ...) {
  .require_pkg("flextable")
  simtab_theme(flextable::flextable(as.data.frame(x), ...))
}

#' @rdname flow
#' @param object A `simtab_flow` object.
#' @exportS3Method ggplot2::autoplot
autoplot.simtab_flow <- function(object, ...) {
  .require_pkg("ggplot2", "autoplot()")
  df <- as.data.frame(object)
  df$stage <- factor(df$stage, levels = rev(df$stage))
  ggplot2::ggplot(df, ggplot2::aes(x = .data[["N"]], y = .data[["stage"]])) +
    ggplot2::geom_col(width = 0.55, fill = "#4C78A8", na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(label = .data[["N"]]), hjust = -0.1, na.rm = TRUE) +
    ggplot2::labs(x = "N", y = NULL) +
    ggplot2::theme_minimal()
}
