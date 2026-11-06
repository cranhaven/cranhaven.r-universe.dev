#' Configure and interrupt queries
#'
#' @param conn An `lb_connection`.
#' @param milliseconds Non-negative timeout in milliseconds. Zero disables the
#'   timeout.
#' @return The supplied `lb_connection` object invisibly. `lb_set_timeout()`
#'   changes its query timeout; `lb_interrupt()` requests interruption of its
#'   running query.
#' @export
lb_set_timeout <- function(conn, milliseconds) {
  .lb_check_conn(conn)
  if (!is.numeric(milliseconds) || length(milliseconds) != 1L ||
      is.na(milliseconds) || !is.finite(milliseconds) || milliseconds < 0 ||
      milliseconds != floor(milliseconds)) {
    rlang::abort("`milliseconds` must be one non-negative integer.",
                 class = "rladybugdb_error_invalid_arg")
  }
  .lb_handle_error(lb_connection_set_timeout_c(conn$ptr, milliseconds))
  invisible(conn)
}

#' @rdname lb_set_timeout
#' @export
lb_interrupt <- function(conn) {
  .lb_check_conn(conn)
  .lb_handle_error(lb_connection_interrupt_c(conn$ptr))
  invisible(conn)
}

.lb_transaction_command <- function(conn, command) {
  result <- lb_execute(conn, command)
  on.exit(lb_close(result), add = TRUE)
  invisible(conn)
}

#' Transaction control
#'
#' @param conn An `lb_connection`.
#' @return The supplied `lb_connection` object invisibly, after starting,
#'   committing, or rolling back its transaction, respectively.
#' @export
lb_begin <- function(conn) .lb_transaction_command(conn, "BEGIN TRANSACTION")

#' @rdname lb_begin
#' @export
lb_commit <- function(conn) .lb_transaction_command(conn, "COMMIT")

#' @rdname lb_begin
#' @export
lb_rollback <- function(conn) .lb_transaction_command(conn, "ROLLBACK")

.lb_extension_name <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !grepl("^[A-Za-z][A-Za-z0-9_]*$", name)) {
    rlang::abort("`name` must be one extension name containing letters, digits, or underscores.",
                 class = "rladybugdb_error_invalid_arg")
  }
  name
}

#' Manage LadybugDB extensions
#'
#' These are thin wrappers around LadybugDB's extension Cypher commands.
#' Remote installation and updating are unavailable in the CRAN build because
#' those operations require a native networking stack. Compatible extensions
#' installed outside R can still be loaded with `lb_load_extension()`.
#'
#' @param conn An `lb_connection`.
#' @param name Official extension name.
#' @return `lb_list_extensions()` returns a `data.frame` describing the
#'   official extensions reported by the engine, with one row per extension.
#'   `lb_load_extension()` returns an `lb_result` containing the LOAD
#'   statement's metadata; release it with [lb_close()].
#'   `lb_install_extension()` and `lb_update_extension()` always raise an error
#'   of class `rladybugdb_error_unsupported` and return no value, because remote
#'   installation and updates are unavailable in the CRAN build.
#' @export
lb_list_extensions <- function(conn) {
  lb_query(conn, "CALL SHOW_OFFICIAL_EXTENSIONS() RETURN *")
}

#' @rdname lb_list_extensions
#' @export
lb_install_extension <- function(conn, name) {
  .lb_extension_name(name)
  rlang::abort(
    paste(
      "Remote extension installation is unavailable in the CRAN build.",
      "Install a compatible LadybugDB extension outside R, then use",
      "`lb_load_extension()`."
    ),
    class = "rladybugdb_error_unsupported"
  )
}

#' @rdname lb_list_extensions
#' @export
lb_load_extension <- function(conn, name) {
  lb_execute(conn, sprintf("LOAD %s", .lb_extension_name(name)))
}

#' @rdname lb_list_extensions
#' @export
lb_update_extension <- function(conn, name) {
  .lb_extension_name(name)
  rlang::abort(
    paste(
      "Remote extension updates are unavailable in the CRAN build.",
      "Update the compatible LadybugDB extension outside R."
    ),
    class = "rladybugdb_error_unsupported"
  )
}
