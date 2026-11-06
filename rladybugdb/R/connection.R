#' Open a LadybugDB database
#'
#' Creates or opens a LadybugDB database. LadybugDB v0.20.4 can read the
#' storage formats created by v0.15.x. For incompatible future formats, export
#' with the original engine and import into a fresh database.
#'
#' @param path File-system path, or `":memory:"` for an ephemeral database.
#' @param read_only Open in read-only mode?
#' @param config Named list of LadybugDB system configuration overrides. Valid
#'   names are `buffer_pool_size`, `max_num_threads`, `enable_compression`,
#'   `max_db_size`, `auto_checkpoint`, `checkpoint_threshold`,
#'   `throw_on_wal_replay_failure`, `enable_checksums`, `enable_multi_writes`,
#'   and `enable_default_hash_index` (`thread_qos` is also accepted on macOS).
#'
#' @return An `lb_database` list containing a native database pointer, its path,
#'   read-only flag, and configuration. It represents an open database that
#'   should be released with [lb_close()] after its connections are closed.
#' @export
lb_database <- function(path = ":memory:", read_only = FALSE, config = list()) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    rlang::abort("`path` must be one non-empty character string.",
                 class = "rladybugdb_error_invalid_arg")
  }
  if (!is.logical(read_only) || length(read_only) != 1L || is.na(read_only)) {
    rlang::abort("`read_only` must be TRUE or FALSE.",
                 class = "rladybugdb_error_invalid_arg")
  }
  if (!is.list(config) || is.null(names(config)) && length(config)) {
    rlang::abort("`config` must be a named list.",
                 class = "rladybugdb_error_invalid_arg")
  }
  if (length(config) && (anyNA(names(config)) || any(!nzchar(names(config))) ||
                         anyDuplicated(names(config)))) {
    rlang::abort("`config` must have non-empty, unique names.",
                 class = "rladybugdb_error_invalid_arg")
  }

  allowed <- c(
    "buffer_pool_size", "max_num_threads", "enable_compression", "max_db_size",
    "auto_checkpoint", "checkpoint_threshold", "throw_on_wal_replay_failure",
    "enable_checksums", "enable_multi_writes", "enable_default_hash_index",
    "thread_qos"
  )
  unknown <- setdiff(names(config), allowed)
  if (length(unknown)) {
    rlang::abort(
      sprintf("Unknown database configuration option(s): %s.",
              paste(unknown, collapse = ", ")),
      class = "rladybugdb_error_invalid_arg"
    )
  }
  if ("thread_qos" %in% names(config) &&
      !identical(Sys.info()[["sysname"]], "Darwin")) {
    rlang::abort("Database option `thread_qos` is available only on macOS.",
                 class = "rladybugdb_error_invalid_arg")
  }
  logical_options <- intersect(names(config), c(
    "enable_compression", "auto_checkpoint", "throw_on_wal_replay_failure",
    "enable_checksums", "enable_multi_writes", "enable_default_hash_index"
  ))
  for (name in logical_options) {
    value <- config[[name]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      rlang::abort(sprintf("Database option `%s` must be TRUE or FALSE.", name),
                   class = "rladybugdb_error_invalid_arg")
    }
  }
  numeric_options <- intersect(names(config), c(
    "buffer_pool_size", "max_num_threads", "max_db_size",
    "checkpoint_threshold", "thread_qos"
  ))
  for (name in numeric_options) {
    value <- config[[name]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < 0 || value != floor(value)) {
      rlang::abort(sprintf("Database option `%s` must be one non-negative integer.", name),
                   class = "rladybugdb_error_invalid_arg")
    }
  }
  config$read_only <- read_only

  ptr <- .lb_handle_error(lb_database_open(enc2utf8(path), config))
  structure(
    list(ptr = ptr, path = path, read_only = read_only, config = config),
    class = "lb_database"
  )
}

#' @export
print.lb_database <- function(x, ...) {
  open <- isTRUE(tryCatch(lb_database_is_open(x$ptr), error = function(e) FALSE))
  cat(sprintf("<lb_database> path=%s read_only=%s state=%s\n",
              x$path, x$read_only, if (open) "open" else "closed"))
  invisible(x)
}

#' Open a connection to a LadybugDB database
#'
#' @param database An `lb_database` object.
#' @param num_threads Positive integer execution thread limit, or `NULL`.
#' @return An `lb_connection` list containing a native connection pointer and
#'   its owning database. Use it to execute queries, and release it with
#'   [lb_close()] after its results are closed.
#' @export
lb_connection <- function(database, num_threads = NULL) {
  if (!inherits(database, "lb_database")) {
    rlang::abort("`database` must be an `lb_database` object.",
                 class = "rladybugdb_error_invalid_arg")
  }
  if (!isTRUE(tryCatch(lb_database_is_open(database$ptr), error = function(e) FALSE))) {
    rlang::abort("The database has been closed.", class = "rladybugdb_error_closed")
  }
  if (!is.null(num_threads) &&
      (!is.numeric(num_threads) || length(num_threads) != 1L || is.na(num_threads) ||
       !is.finite(num_threads) || num_threads < 1 || num_threads != floor(num_threads))) {
    rlang::abort("`num_threads` must be NULL or one positive integer.",
                 class = "rladybugdb_error_invalid_arg")
  }
  if (!is.null(num_threads) && num_threads > .Machine$integer.max) {
    rlang::abort("`num_threads` exceeds R's supported integer range.",
                 class = "rladybugdb_error_invalid_arg")
  }
  n <- if (is.null(num_threads)) 0L else as.integer(num_threads)
  ptr <- .lb_handle_error(lb_connection_create(database$ptr, n))
  structure(list(ptr = ptr, database = database), class = "lb_connection")
}

#' @export
print.lb_connection <- function(x, ...) {
  open <- isTRUE(tryCatch(lb_connection_is_open(x$ptr), error = function(e) FALSE))
  cat(sprintf("<lb_connection> db=%s state=%s\n", x$database$path,
              if (open) "open" else "closed"))
  invisible(x)
}

#' Close a LadybugDB result, connection, or database
#'
#' Closing an object more than once is harmless. A connection cannot be closed
#' while it owns open results, and a database cannot be closed while it owns
#' open connections.
#'
#' @param x An `lb_result`, `lb_connection`, or `lb_database`.
#' @param ... Unused.
#' @return Invisible `NULL`, called for the side effect of releasing the
#'   object's native resources.
#' @export
lb_close <- function(x, ...) UseMethod("lb_close")

#' @export
lb_close.lb_result <- function(x, ...) {
  if (!inherits(x, "lb_result")) return(invisible(NULL))
  .lb_handle_error(lb_result_close(x$ptr))
  invisible(NULL)
}

#' @export
lb_close.lb_connection <- function(x, ...) {
  if (!inherits(x, "lb_connection")) return(invisible(NULL))
  gc(verbose = FALSE)
  .lb_handle_error(lb_connection_close(x$ptr))
  invisible(NULL)
}

#' @export
lb_close.lb_database <- function(x, ...) {
  if (!inherits(x, "lb_database")) return(invisible(NULL))
  .lb_handle_error(lb_database_close(x$ptr))
  invisible(NULL)
}

#' Run code with an automatically closed LadybugDB connection
#'
#' @param path Database path.
#' @param code Code evaluated with `conn` bound to the open connection.
#' @param ... Passed to [lb_database()].
#' @param num_threads Passed to [lb_connection()].
#' @return The value produced by `code`, with its class and visibility
#'   preserved. The connection and database are closed before returning.
#' @export
with_lb_connection <- function(path = ":memory:", code, ..., num_threads = NULL) {
  database <- lb_database(path, ...)
  conn <- tryCatch(
    lb_connection(database, num_threads = num_threads),
    error = function(e) {
      lb_close(database)
      stop(e)
    }
  )
  on.exit({
    lb_close(conn)
    lb_close(database)
  }, add = TRUE)
  eval(substitute(code), envir = list(conn = conn), enclos = parent.frame())
}
