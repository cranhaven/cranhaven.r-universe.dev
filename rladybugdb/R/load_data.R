#' Load an Arrow object into a LadybugDB table
#'
#' Registers a temporary Arrow-backed table through the Arrow C Data Interface,
#' copies its rows into an existing native node table, and unregisters it. No
#' data-frame or CSV conversion is performed.
#'
#' @param conn An `lb_connection`.
#' @param value An Arrow Table, RecordBatch, or object accepted by
#'   `arrow::as_record_batch()`.
#' @param table Existing target node table name.
#' @return An `lb_result` containing the COPY statement's metadata after
#'   loading the Arrow rows into the target table. Release it with [lb_close()].
#' @export
lb_copy_from_arrow <- function(conn, value, table) {
  .lb_check_conn(conn)
  target <- .lb_quote_identifier(table, "table")
  if (!requireNamespace("arrow", quietly = TRUE)) {
    rlang::abort("Package `arrow` is required for native Arrow loading.",
                 class = "rladybugdb_error_missing_pkg")
  }

  batch <- tryCatch(
    arrow::as_record_batch(value),
    error = function(e) rlang::abort(
      "`value` could not be converted to an Arrow RecordBatch.",
      class = "rladybugdb_error_invalid_arg", parent = e
    )
  )
  temporary_name <- sprintf("__rladybugdb_arrow_%s_%s", Sys.getpid(),
                            sample.int(.Machine$integer.max, 1L))
  temporary <- .lb_quote_identifier(temporary_name)
  pointers <- lb_arrow_allocate_c()
  batch$export_to_c(pointers$array, pointers$schema)

  registration <- .lb_handle_error(lb_connection_create_arrow_table_c(
    conn$ptr, temporary_name, pointers$array, pointers$schema
  ))
  registration <- new_lb_result(registration, conn, "register Arrow table")
  lb_close(registration)

  on.exit({
    drop_ptr <- tryCatch(lb_connection_drop_arrow_table_c(conn$ptr, temporary_name),
                         error = function(e) NULL)
    if (!is.null(drop_ptr)) {
      drop_result <- new_lb_result(drop_ptr, conn, "drop Arrow table")
      try(lb_close(drop_result), silent = TRUE)
    }
  }, add = TRUE)

  lb_execute(conn, sprintf("COPY %s FROM (MATCH (n:%s) RETURN n.*)",
                           target, temporary))
}

#' Load an R data frame into a LadybugDB table
#'
#' Uses native Arrow transport when the optional `arrow` package is available.
#' Otherwise it uses the explicit CSV loader as a compatibility fallback.
#'
#' @param conn An `lb_connection`.
#' @param df A data frame.
#' @param table Existing target table name.
#' @return An `lb_result` containing the COPY statement's metadata after
#'   loading the data-frame rows into the target table. Release it with
#'   [lb_close()].
#' @export
lb_copy_from_df <- function(conn, df, table) {
  .lb_check_conn(conn)
  if (!is.data.frame(df)) {
    rlang::abort("`df` must be a data.frame.", class = "rladybugdb_error_invalid_arg")
  }
  .lb_quote_identifier(table, "table")
  if (requireNamespace("arrow", quietly = TRUE)) {
    return(lb_copy_from_arrow(conn, arrow::Table$create(df), table))
  }

  warning("Package `arrow` is unavailable; using the CSV fallback, which has lower type fidelity.",
          call. = FALSE)
  temporary <- tempfile(fileext = ".csv")
  on.exit(unlink(temporary), add = TRUE)
  utils::write.csv(df, temporary, row.names = FALSE, na = "")
  lb_copy_from_csv(conn, temporary, table)
}

#' Load a CSV file into a LadybugDB table
#'
#' @param conn An `lb_connection`.
#' @param path Path to a CSV file.
#' @param table Existing target table name.
#' @param header Does the file contain a header?
#' @param delim One-character delimiter.
#' @param ... Named COPY options. Values must be scalar logical, numeric, or
#'   character values; option names are validated identifiers.
#' @return An `lb_result` containing the COPY statement's metadata after
#'   loading the CSV rows into the target table. Release it with [lb_close()].
#' @export
lb_copy_from_csv <- function(conn, path, table, header = TRUE, delim = ",", ...) {
  .lb_check_conn(conn)
  target <- .lb_quote_identifier(table, "table")
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    rlang::abort("`path` must be one non-empty string.",
                 class = "rladybugdb_error_invalid_arg")
  }
  if (!file.exists(path)) {
    rlang::abort(sprintf("CSV file does not exist: %s", path),
                 class = "rladybugdb_error_invalid_arg")
  }
  if (!is.logical(header) || length(header) != 1L || is.na(header)) {
    rlang::abort("`header` must be TRUE or FALSE.",
                 class = "rladybugdb_error_invalid_arg")
  }
  if (!is.character(delim) || length(delim) != 1L || is.na(delim) ||
      nchar(delim, type = "chars") != 1L) {
    rlang::abort("`delim` must be exactly one character.",
                 class = "rladybugdb_error_invalid_arg")
  }

  extra <- list(...)
  if (length(extra) && (is.null(names(extra)) || any(!nzchar(names(extra))) ||
                        anyDuplicated(names(extra)))) {
    rlang::abort("COPY options must have non-empty, unique names.",
                 class = "rladybugdb_error_invalid_arg")
  }
  option_name <- function(x) {
    if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", x)) {
      rlang::abort(sprintf("Invalid COPY option name `%s`.", x),
                   class = "rladybugdb_error_invalid_arg")
    }
    tolower(x)
  }
  option_value <- function(x) {
    if (length(x) != 1L || is.na(x)) {
      rlang::abort("COPY option values must be non-missing scalars.",
                   class = "rladybugdb_error_invalid_arg")
    }
    if (is.logical(x)) return(if (x) "true" else "false")
    if (is.numeric(x) && is.finite(x)) return(format(x, scientific = FALSE, trim = TRUE))
    if (is.character(x)) return(.lb_quote_string(x))
    rlang::abort("Unsupported COPY option value.", class = "rladybugdb_error_invalid_arg")
  }

  options <- c(
    sprintf("header=%s", if (header) "true" else "false"),
    sprintf("delim=%s", .lb_quote_string(delim, "delim"))
  )
  if (length(extra)) {
    options <- c(options, vapply(seq_along(extra), function(i) {
      sprintf("%s=%s", option_name(names(extra)[i]), option_value(extra[[i]]))
    }, character(1)))
  }
  normalized_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  query <- sprintf("COPY %s FROM %s (%s)", target,
                   .lb_quote_string(normalized_path, "path"),
                   paste(options, collapse = ", "))
  lb_execute(conn, query)
}
