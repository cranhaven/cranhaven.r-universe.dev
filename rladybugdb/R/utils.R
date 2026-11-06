#' Assert that an object is an lb_connection
#'
#' @param conn Object to check.
#' @return The supplied `lb_connection` object invisibly if it is open.
#'   Raises a typed error for an invalid or closed connection.
#' @keywords internal
.lb_check_conn <- function(conn) {
  if (!inherits(conn, "lb_connection")) {
    rlang::abort(
      "`conn` must be an `lb_connection` object created by `lb_connection()`.",
      class = "rladybugdb_error_invalid_conn"
    )
  }
  if (!isTRUE(tryCatch(lb_connection_is_open(conn$ptr), error = function(e) FALSE))) {
    rlang::abort(
      "`conn` has been closed.",
      class = c("rladybugdb_error_closed", "rladybugdb_error_invalid_conn")
    )
  }
  invisible(conn)
}

.lb_check_result <- function(result) {
  if (!inherits(result, "lb_result")) {
    rlang::abort("`result` must be an `lb_result` object.",
                 class = "rladybugdb_error_invalid_result")
  }
  if (!isTRUE(tryCatch(lb_result_is_open(result$ptr), error = function(e) FALSE))) {
    rlang::abort("The query result has been closed.",
                 class = "rladybugdb_error_closed")
  }
  invisible(result)
}

.lb_quote_identifier <- function(x, arg = "identifier") {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    rlang::abort(sprintf("`%s` must be one non-empty string.", arg),
                 class = "rladybugdb_error_invalid_arg")
  }
  paste0("`", gsub("`", "``", enc2utf8(x), fixed = TRUE), "`")
}

.lb_quote_string <- function(x, arg = "value") {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    rlang::abort(sprintf("`%s` must be one non-missing string.", arg),
                 class = "rladybugdb_error_invalid_arg")
  }
  escaped <- gsub("\\", "\\\\", enc2utf8(x), fixed = TRUE)
  escaped <- gsub("'", "\\'", escaped, fixed = TRUE)
  paste0("'", escaped, "'")
}

.lb_bigint_mode <- function() {
  mode <- getOption("rladybugdb.bigint", "double")
  if (!is.character(mode) || length(mode) != 1L ||
      !mode %in% c("double", "integer64", "character")) {
    rlang::abort(
      "Option `rladybugdb.bigint` must be 'double', 'integer64', or 'character'.",
      class = "rladybugdb_error_invalid_option"
    )
  }
  if (identical(mode, "integer64") &&
      !requireNamespace("bit64", quietly = TRUE)) {
    rlang::abort(
      "Package `bit64` is required when `rladybugdb.bigint` is 'integer64'.",
      class = "rladybugdb_error_missing_pkg"
    )
  }
  mode
}

.lb_as_data_frame <- function(raw) {
  for (i in seq_along(raw)) {
    if (is.list(raw[[i]]) && !inherits(raw[[i]], "data.frame")) raw[[i]] <- I(raw[[i]])
  }
  as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE,
                optional = TRUE)
}

#' Handle C++ exceptions from LadybugDB
#'
#' Wraps a call expression, catches any condition thrown by the Rcpp layer,
#' and re-throws as a typed R error with class `rladybugdb_error_query`.
#'
#' @param expr Expression to evaluate.
#' @return The value of `expr`, with its class and visibility preserved.
#'   Raises a typed error if evaluating the expression fails.
#' @keywords internal
.lb_handle_error <- function(expr) {
  tryCatch(
    expr,
    error = function(e) {
      rlang::abort(
        conditionMessage(e),
        class = c("rladybugdb_error_query", "rladybugdb_error"),
        parent = e
      )
    }
  )
}
