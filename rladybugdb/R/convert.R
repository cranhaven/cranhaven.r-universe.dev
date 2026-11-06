#' Convert a LadybugDB result to a data frame
#'
#' Materialization preserves the current streaming cursor, so it is repeatable
#' and does not change subsequent calls to [lb_fetch()].
#'
#' @param x An `lb_result`.
#' @param ... Unused.
#' @return A `data.frame` containing all query rows, with one column per
#'   returned expression. Nested database values are stored in list columns.
#' @export
as.data.frame.lb_result <- function(x, ...) {
  .lb_check_result(x)
  raw <- .lb_handle_error(
    lb_result_fetch(x$ptr, Inf, TRUE, .lb_bigint_mode())
  )
  .lb_as_data_frame(raw)
}

#' Convert a LadybugDB result to an Arrow Table
#'
#' @param x An `lb_result`.
#' @param ... Unused.
#' @return An `arrow::Table` containing all query rows and columns, backed by
#'   LadybugDB's Arrow C Data export. The streaming cursor is preserved.
#' @export
as_arrow_table <- function(x, ...) UseMethod("as_arrow_table")

#' @export
as_arrow_table.lb_result <- function(x, ...) {
  .lb_check_result(x)
  if (!requireNamespace("arrow", quietly = TRUE)) {
    rlang::abort(
      "Package `arrow` is required for Arrow conversion.",
      class = "rladybugdb_error_missing_pkg"
    )
  }
  pair <- .lb_handle_error(lb_result_fetch_arrow_c(x$ptr, Inf, TRUE))
  batch <- arrow::RecordBatch$import_from_c(pair$array, pair$schema)
  arrow::Table$create(batch)
}

#' Fetch a chunk as an Arrow Table
#'
#' @param result An `lb_result`.
#' @param n Maximum rows to fetch. `Inf` fetches all remaining rows.
#' @return An `arrow::Table` containing up to `n` rows from the current cursor
#'   position. The cursor advances by the number of rows returned.
#' @export
lb_fetch_arrow <- function(result, n = Inf) {
  .lb_check_result(result)
  if (!requireNamespace("arrow", quietly = TRUE)) {
    rlang::abort("Package `arrow` is required.",
                 class = "rladybugdb_error_missing_pkg")
  }
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 0) {
    rlang::abort("`n` must be one non-negative number or Inf.",
                 class = "rladybugdb_error_invalid_arg")
  }
  pair <- .lb_handle_error(lb_result_fetch_arrow_c(result$ptr, n, FALSE))
  batch <- arrow::RecordBatch$import_from_c(pair$array, pair$schema)
  arrow::Table$create(batch)
}

#' Convert a LadybugDB result to a tibble
#'
#' @param x An `lb_result`.
#' @param ... Passed to [as.data.frame()].
#' @return A tibble (classes `tbl_df`, `tbl`, and `data.frame`) containing all
#'   query rows, with one column per returned expression.
#' @export
as_tibble <- function(x, ...) UseMethod("as_tibble")

#' @export
as_tibble.lb_result <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    rlang::abort("Package `tibble` is required.",
                 class = "rladybugdb_error_missing_pkg")
  }
  tibble::as_tibble(as.data.frame(x, ...))
}

#' Format a LadybugDB interval
#'
#' @param x An `lb_interval` value.
#' @param ... Unused.
#' @return `format()` returns a character string describing the interval's
#'   months, days, and microseconds. `print()` displays that description and
#'   returns the supplied `lb_interval` object invisibly.
#' @examples
#' interval <- structure(
#'   list(months = 1L, days = 2L, microseconds = 3000000),
#'   class = "lb_interval"
#' )
#' format(interval)
#' print(interval)
#' @export
format.lb_interval <- function(x, ...) {
  sprintf("%s months, %s days, %s microseconds",
          x[["months"]], x[["days"]], x[["microseconds"]])
}

#' @rdname format.lb_interval
#' @export
print.lb_interval <- function(x, ...) {
  cat("<lb_interval> ", format(x), "\n", sep = "")
  invisible(x)
}
