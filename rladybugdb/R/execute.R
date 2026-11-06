#' Execute a Cypher query
#'
#' Sends a Cypher query string to LadybugDB and returns an `lb_result` object.
#' For DDL or DML statements (CREATE TABLE, CREATE, MERGE, …) the result
#' contains metadata; for MATCH/RETURN it contains rows.
#'
#' @param conn An `lb_connection` object from [lb_connection()].
#' @param query A single character string containing a Cypher query.
#' @param parameters A named list of query parameters for parameterised queries.
#'   `NULL` (default) means no parameters.
#'
#' @return An `lb_result` list containing a native result pointer, its owning
#'   connection, query text, column names and types, and row count. It represents
#'   query rows or statement metadata and must be released with [lb_close()].
#'
#' @examples
#' with_lb_connection(":memory:", {
#'   lb_close(lb_execute(
#'     conn, "CREATE NODE TABLE Person (name STRING, PRIMARY KEY(name))"
#'   ))
#'   result <- lb_execute(conn, "CREATE (:Person {name: $name})",
#'                        parameters = list(name = "Alice"))
#'   print(result)
#'   lb_close(result)
#' }, num_threads = 2)
#'
#' @export
lb_execute <- function(conn, query, parameters = NULL) {
  .lb_check_conn(conn)

  if (!is.character(query) || length(query) != 1L) {
    rlang::abort("`query` must be a single character string.",
                 class = "rladybugdb_error_invalid_arg")
  }
  if (is.na(query) || !nzchar(query)) {
    rlang::abort("`query` must not be missing or empty.",
                 class = "rladybugdb_error_invalid_arg")
  }

  ptr <- if (is.null(parameters)) {
    .lb_handle_error(lb_connection_execute(conn$ptr, query))
  } else {
    if (!is.list(parameters)) {
      rlang::abort("`parameters` must be a named list or NULL.",
                   class = "rladybugdb_error_invalid_arg")
    }
    parameter_names <- names(parameters)
    if (is.null(parameter_names) || length(parameter_names) != length(parameters) ||
        anyNA(parameter_names) || any(!nzchar(parameter_names)) || anyDuplicated(parameter_names)) {
      rlang::abort(
        "`parameters` must have non-empty, unique names.",
        class = "rladybugdb_error_invalid_arg"
      )
    }
    tryCatch(
      lb_connection_execute_params(conn$ptr, enc2utf8(query), parameters),
      error = function(e) {
        labels <- paste(sprintf("`%s`", parameter_names), collapse = ", ")
        message <- sprintf("Failed to prepare or bind parameter(s) %s: %s",
                           labels, conditionMessage(e))
        rlang::abort(
          message,
          class = c("rladybugdb_error_bind", "rladybugdb_error_query",
                    "rladybugdb_error"),
          parent = e
        )
      }
    )
  }

  new_lb_result(ptr, conn = conn, query = query)
}

#' Execute a Cypher query and return a data frame
#'
#' Convenience wrapper around [lb_execute()] that immediately converts the
#' result to an R `data.frame`. Equivalent to
#' `as.data.frame(lb_execute(conn, query, ...))`.
#'
#' @inheritParams lb_execute
#' @param ... Passed to [as.data.frame.lb_result()].
#'
#' @return A `data.frame` containing all query rows, with one column per
#'   returned expression. The underlying native result is closed automatically.
#'
#' @examples
#' with_lb_connection(":memory:", {
#'   lb_close(lb_execute(
#'     conn,
#'     "CREATE NODE TABLE Person (name STRING, age INT64, PRIMARY KEY(name))"
#'   ))
#'   lb_close(lb_execute(conn, "CREATE (:Person {name: 'Alice', age: 30})"))
#'   lb_query(conn, "MATCH (p:Person) RETURN p.name AS name, p.age AS age")
#' }, num_threads = 2)
#'
#' @export
lb_query <- function(conn, query, parameters = NULL, ...) {
  result <- lb_execute(conn, query, parameters = parameters)
  on.exit(lb_close(result), add = TRUE)
  as.data.frame(result, ...)
}
