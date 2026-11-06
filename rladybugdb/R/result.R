#' LadybugDB query result
#'
#' A native query result with an independent, resettable cursor. Results keep
#' their connection and database reachable until [lb_close()] is called.
#'
#' @param ptr Native result pointer.
#' @param conn Owning `lb_connection`.
#' @param query Original query text.
#' @param parent Parent result for a multi-statement query, if any.
#' @return An `lb_result` list containing the native result pointer, its owning
#'   connection, original query, optional parent result, column names and types,
#'   and the total row count.
#' @keywords internal
new_lb_result <- function(ptr, conn = NULL, query = NULL, parent = NULL) {
  col_names <- tryCatch(lb_result_column_names(ptr), error = function(e) character())
  col_types <- tryCatch(lb_result_column_types(ptr), error = function(e) character())
  num_rows <- tryCatch(lb_result_num_tuples(ptr), error = function(e) NA_real_)
  structure(
    list(ptr = ptr, conn = conn, query = query, parent = parent,
         col_names = col_names, col_types = col_types, num_rows = num_rows),
    class = "lb_result"
  )
}

#' Fetch rows from a LadybugDB result
#'
#' @param result An `lb_result`.
#' @param n Maximum rows to fetch. `Inf` fetches the remainder.
#' @return `lb_fetch()` returns a `data.frame` with up to `n` rows from the
#'   current cursor position and one column per returned expression. Repeated
#'   calls advance the cursor. `lb_has_next()` returns a logical scalar that is
#'   `TRUE` when unfetched rows remain. `lb_reset()` returns the supplied
#'   `lb_result` invisibly after moving its cursor back to the first row.
#' @export
lb_fetch <- function(result, n = 10000) {
  .lb_check_result(result)
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 0) {
    rlang::abort("`n` must be one non-negative number or Inf.",
                 class = "rladybugdb_error_invalid_arg")
  }
  raw <- .lb_handle_error(lb_result_fetch(result$ptr, n, FALSE, .lb_bigint_mode()))
  .lb_as_data_frame(raw)
}

#' @rdname lb_fetch
#' @export
lb_has_next <- function(result) {
  .lb_check_result(result)
  isTRUE(.lb_handle_error(lb_result_has_next_c(result$ptr)))
}

#' @rdname lb_fetch
#' @export
lb_reset <- function(result) {
  .lb_check_result(result)
  .lb_handle_error(lb_result_reset_c(result$ptr))
  invisible(result)
}

#' Inspect result metadata
#'
#' @param result An `lb_result`.
#' @return A named list with character vectors `column_names` and
#'   `column_types`; numeric scalars `num_columns`, `num_tuples` (total rows),
#'   and `rows_fetched` (current cursor position); and logical scalars
#'   `complete` (no unfetched rows remain) and `has_next_result` (another
#'   statement's result is available).
#' @export
lb_result_info <- function(result) {
  .lb_check_result(result)
  .lb_handle_error(lb_result_info_c(result$ptr))
}

#' Return query timing information
#'
#' @param result An `lb_result`.
#' @return A named list with numeric scalars `compiling_time_ms` and
#'   `execution_time_ms`, measuring the query's compilation and execution times
#'   in milliseconds.
#' @export
lb_query_summary <- function(result) {
  .lb_check_result(result)
  .lb_handle_error(lb_result_summary_c(result$ptr))
}

#' Retrieve the next result from a multi-statement query
#'
#' @param result An `lb_result`.
#' @return An `lb_result` representing the next statement's rows or metadata,
#'   or `NULL` if no further result is available. Close each returned result
#'   with [lb_close()].
#' @export
lb_next_result <- function(result) {
  .lb_check_result(result)
  ptr <- .lb_handle_error(lb_result_next_result_c(result$ptr))
  if (is.null(ptr)) return(NULL)
  new_lb_result(ptr, conn = result$conn, query = result$query, parent = result)
}

#' @export
print.lb_result <- function(x, ..., n = getOption("rladybugdb.print_max", 10L)) {
  .lb_check_result(x)
  info <- lb_result_info(x)
  cat(sprintf("<lb_result> [%s x %d]\n", format(info$num_tuples, scientific = FALSE),
              info$num_columns))
  if (info$num_columns && info$num_tuples) {
    preview <- .lb_as_data_frame(.lb_handle_error(
      lb_result_fetch(x$ptr, n, TRUE, .lb_bigint_mode())
    ))
    print(preview, ...)
    if (info$num_tuples > n) cat(sprintf("# ... with %s more rows\n",
                                         format(info$num_tuples - n, scientific = FALSE)))
  } else if (info$num_columns) {
    cat("# columns: ", paste(x$col_names, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

#' @export
format.lb_result <- function(x, ..., n = getOption("rladybugdb.print_max", 10L)) {
  .lb_check_result(x)
  info <- lb_result_info(x)
  header <- sprintf("<lb_result> [%s x %d]", format(info$num_tuples, scientific = FALSE),
                    info$num_columns)
  if (!info$num_columns || !info$num_tuples) return(header)
  preview <- .lb_as_data_frame(.lb_handle_error(
    lb_result_fetch(x$ptr, n, TRUE, .lb_bigint_mode())
  ))
  paste(c(header, utils::capture.output(print(preview, ...))), collapse = "\n")
}
