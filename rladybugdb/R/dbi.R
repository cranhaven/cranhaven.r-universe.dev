#' LadybugDB DBI driver
#'
#' @slot state Internal driver state.
#' @export
methods::setClass("LadybugDriver", contains = "DBIDriver",
                  slots = c(state = "environment"))

#' LadybugDB DBI connection
#'
#' @slot database Native database object.
#' @slot connection Native connection object.
#' @slot state Mutable connection state.
#' @slot bigint INT64 conversion policy.
#' @export
methods::setClass(
  "LadybugConnection",
  contains = "DBIConnection",
  slots = c(database = "ANY", connection = "ANY", state = "environment",
            bigint = "character")
)

#' LadybugDB DBI result
#'
#' @slot connection Owning DBI connection.
#' @slot state Mutable result state.
#' @slot statement Original Cypher statement.
#' @export
methods::setClass(
  "LadybugResult",
  contains = "DBIResult",
  slots = c(connection = "LadybugConnection", state = "environment",
            statement = "character")
)

#' Create a LadybugDB DBI driver
#'
#' @return A `LadybugDriver` S4 object, inheriting from `DBIDriver`, for use with
#'   [DBI::dbConnect()] to open a LadybugDB database.
#' @export
Ladybug <- function() methods::new("LadybugDriver", state = new.env(parent = emptyenv()))

#' @export
methods::setMethod("show", "LadybugDriver", function(object) {
  cat("<LadybugDriver> LadybugDB ", ladybugdb_version(), "\n", sep = "")
})

#' @export
methods::setMethod("show", "LadybugConnection", function(object) {
  cat("<LadybugConnection> ", object@database$path, " (",
      if (DBI::dbIsValid(object)) "open" else "closed", ")\n", sep = "")
})

#' @export
methods::setMethod("show", "LadybugResult", function(object) {
  info <- DBI::dbGetInfo(object)
  cat("<LadybugResult> rows=", info$row.count, " complete=", info$completed, "\n", sep = "")
})

#' @export
methods::setMethod("dbConnect", "LadybugDriver",
  function(drv, dbname = ":memory:", ..., read_only = FALSE,
           num_threads = NULL,
           bigint = c("double", "numeric", "integer64", "character")) {
    bigint <- match.arg(bigint)
    if (identical(bigint, "numeric")) bigint <- "double"
    database <- lb_database(dbname, read_only = read_only, config = list(...))
    connection <- tryCatch(
      lb_connection(database, num_threads = num_threads),
      error = function(e) {
        lb_close(database)
        stop(e)
      }
    )
    state <- new.env(parent = emptyenv())
    state$valid <- TRUE
    state$results <- list()
    methods::new("LadybugConnection", database = database,
                 connection = connection, state = state, bigint = bigint)
  }
)

#' @export
methods::setMethod("dbDisconnect", "LadybugConnection", function(conn, ...) {
  if (!isTRUE(conn@state$valid)) return(TRUE)
  for (result_state in conn@state$results) {
    if (is.environment(result_state) && isTRUE(result_state$valid)) {
      if (!is.null(result_state$result)) {
        try(lb_close(result_state$result), silent = TRUE)
      }
      result_state$valid <- FALSE
    }
  }
  conn@state$results <- list()
  lb_close(conn@connection)
  lb_close(conn@database)
  conn@state$valid <- FALSE
  TRUE
})

#' @export
methods::setMethod("dbIsValid", "LadybugConnection", function(dbObj, ...) {
  isTRUE(dbObj@state$valid) &&
    isTRUE(tryCatch(lb_connection_is_open(dbObj@connection$ptr), error = function(e) FALSE))
})

#' @export
methods::setMethod("dbIsValid", "LadybugDriver", function(dbObj, ...) TRUE)

#' @export
methods::setMethod("dbIsValid", "LadybugResult", function(dbObj, ...) {
  isTRUE(dbObj@state$valid) && DBI::dbIsValid(dbObj@connection) &&
    (isTRUE(dbObj@state$pending) ||
      (!is.null(dbObj@state$result) &&
       isTRUE(tryCatch(lb_result_is_open(dbObj@state$result$ptr),
                       error = function(e) FALSE))))
})

.dbi_check_connection <- function(conn) {
  if (!DBI::dbIsValid(conn)) stop("The LadybugDB DBI connection is closed.", call. = FALSE)
}

.dbi_check_result <- function(result) {
  if (!DBI::dbIsValid(result)) stop("The LadybugDB DBI result is closed.", call. = FALSE)
}

.new_dbi_result <- function(conn, statement, parameters = NULL) {
  pending <- is.null(parameters) &&
    grepl("\\$[A-Za-z_][A-Za-z0-9_]*", statement, perl = TRUE)
  native <- if (pending) NULL else {
    lb_execute(conn@connection, statement, parameters = parameters)
  }
  state <- new.env(parent = emptyenv())
  state$result <- native
  state$valid <- TRUE
  state$pending <- pending
  state$fetched <- 0
  state$parameters <- parameters
  conn@state$results[[length(conn@state$results) + 1L]] <- state
  methods::new("LadybugResult", connection = conn, state = state,
               statement = enc2utf8(statement))
}

#' @export
methods::setMethod("dbSendQuery", signature(conn = "LadybugConnection", statement = "character"),
  function(conn, statement, ..., params = NULL) {
    .dbi_check_connection(conn)
    .new_dbi_result(conn, statement, params)
  }
)

#' @export
methods::setMethod("dbSendStatement", signature(conn = "LadybugConnection", statement = "character"),
  function(conn, statement, ..., params = NULL) {
    .dbi_check_connection(conn)
    .new_dbi_result(conn, statement, params)
  }
)

#' @export
methods::setMethod("dbFetch", "LadybugResult", function(res, n = -1, ...) {
  .dbi_check_result(res)
  if (isTRUE(res@state$pending)) {
    stop("This parameterized result is pending; call `dbBind()` before fetching.",
         call. = FALSE)
  }
  old <- getOption("rladybugdb.bigint")
  on.exit(options(rladybugdb.bigint = old), add = TRUE)
  options(rladybugdb.bigint = res@connection@bigint)
  amount <- if (identical(n, -1L) || identical(n, -1)) Inf else n
  out <- lb_fetch(res@state$result, amount)
  res@state$fetched <- res@state$fetched + nrow(out)
  out
})

#' @export
methods::setMethod("dbHasCompleted", "LadybugResult", function(res, ...) {
  .dbi_check_result(res)
  if (isTRUE(res@state$pending)) return(FALSE)
  !lb_has_next(res@state$result)
})

#' @export
methods::setMethod("dbClearResult", "LadybugResult", function(res, ...) {
  if (!isTRUE(res@state$valid)) return(TRUE)
  if (!is.null(res@state$result)) lb_close(res@state$result)
  res@state$valid <- FALSE
  res@state$pending <- FALSE
  states <- res@connection@state$results
  keep <- !vapply(states, identical, logical(1), y = res@state)
  res@connection@state$results <- states[keep]
  TRUE
})

#' @export
methods::setMethod("dbBind", "LadybugResult", function(res, params, ...) {
  .dbi_check_result(res)
  if (!is.null(res@state$result)) lb_close(res@state$result)
  res@state$result <- NULL
  res@state$pending <- TRUE
  res@state$result <- lb_execute(
    res@connection@connection,
    res@statement,
    parameters = params
  )
  res@state$pending <- FALSE
  res@state$parameters <- params
  res@state$fetched <- 0
  invisible(res)
})

#' @export
methods::setMethod("dbGetQuery", signature(conn = "LadybugConnection", statement = "character"),
  function(conn, statement, ..., params = NULL) {
    result <- DBI::dbSendQuery(conn, statement, ..., params = params)
    on.exit(DBI::dbClearResult(result), add = TRUE)
    DBI::dbFetch(result, n = -1)
  }
)

#' @export
methods::setMethod("dbExecute", signature(conn = "LadybugConnection", statement = "character"),
  function(conn, statement, ..., params = NULL) {
    result <- DBI::dbSendStatement(conn, statement, ..., params = params)
    on.exit(DBI::dbClearResult(result), add = TRUE)
    as.numeric(DBI::dbGetRowsAffected(result))
  }
)

#' @export
methods::setMethod("dbGetRowCount", "LadybugResult", function(res, ...) {
  as.numeric(res@state$fetched)
})

#' @export
methods::setMethod("dbGetRowsAffected", "LadybugResult", function(res, ...) {
  0
})

#' @export
methods::setMethod("dbColumnInfo", "LadybugResult", function(res, ...) {
  .dbi_check_result(res)
  if (isTRUE(res@state$pending)) {
    stop("Column metadata is unavailable until `dbBind()` is called.", call. = FALSE)
  }
  info <- lb_result_info(res@state$result)
  data.frame(name = info$column_names, type = info$column_types,
             stringsAsFactors = FALSE)
})

#' @export
methods::setMethod("dbGetInfo", "LadybugDriver", function(dbObj, ...) {
  list(driver.version = as.character(utils::packageVersion("rladybugdb")),
       client.version = ladybugdb_version())
})

#' @export
methods::setMethod("dbGetInfo", "LadybugConnection", function(dbObj, ...) {
  list(dbname = dbObj@database$path, read.only = dbObj@database$read_only,
       bigint = dbObj@bigint, valid = DBI::dbIsValid(dbObj),
       serverVersion = ladybugdb_version())
})

#' @export
methods::setMethod("dbGetInfo", "LadybugResult", function(dbObj, ...) {
  if (!isTRUE(dbObj@state$valid)) {
    return(list(statement = dbObj@statement, row.count = dbObj@state$fetched,
                completed = TRUE))
  }
  if (isTRUE(dbObj@state$pending)) {
    return(list(statement = dbObj@statement, row.count = dbObj@state$fetched,
                completed = FALSE, pending = TRUE))
  }
  info <- lb_result_info(dbObj@state$result)
  c(list(statement = dbObj@statement, row.count = dbObj@state$fetched,
         completed = info$complete), info)
})

#' @export
methods::setMethod("dbIsReadOnly", "LadybugConnection", function(dbObj, ...) {
  isTRUE(dbObj@database$read_only)
})

#' @export
methods::setMethod("dbBegin", "LadybugConnection", function(conn, ...) {
  .dbi_check_connection(conn)
  lb_begin(conn@connection)
  TRUE
})

#' @export
methods::setMethod("dbCommit", "LadybugConnection", function(conn, ...) {
  .dbi_check_connection(conn)
  lb_commit(conn@connection)
  TRUE
})

#' @export
methods::setMethod("dbRollback", "LadybugConnection", function(conn, ...) {
  .dbi_check_connection(conn)
  lb_rollback(conn@connection)
  TRUE
})

#' @export
methods::setMethod("dbQuoteIdentifier", signature(conn = "LadybugConnection", x = "character"),
  function(conn, x, ...) DBI::SQL(vapply(x, .lb_quote_identifier, character(1))))

#' @export
methods::setMethod("dbQuoteString", signature(conn = "LadybugConnection", x = "character"),
  function(conn, x, ...) DBI::SQL(vapply(x, .lb_quote_string, character(1))))

#' @export
methods::setMethod("dbListTables", "LadybugConnection", function(conn, ...) {
  .dbi_check_connection(conn)
  out <- DBI::dbGetQuery(conn, "CALL SHOW_TABLES() RETURN *")
  if (!ncol(out)) character() else if ("name" %in% names(out)) {
    as.character(out[["name"]])
  } else {
    as.character(out[[1L]])
  }
})

#' @export
methods::setMethod("dbListFields", signature(conn = "LadybugConnection", name = "character"),
  function(conn, name, ...) {
    .dbi_check_connection(conn)
    query <- sprintf("CALL TABLE_INFO(%s) RETURN *", .lb_quote_string(name, "name"))
    out <- DBI::dbGetQuery(conn, query)
    if ("name" %in% names(out)) as.character(out[["name"]]) else character()
  })

#' @export
methods::setMethod("dbExistsTable", signature(conn = "LadybugConnection", name = "character"),
  function(conn, name, ...) name %in% DBI::dbListTables(conn))

#' @export
methods::setMethod("dbDataType", signature(dbObj = "LadybugDriver", obj = "ANY"),
  function(dbObj, obj, ...) {
    if (is.data.frame(obj)) {
      return(vapply(obj, function(column) DBI::dbDataType(dbObj, column),
                    character(1)))
    }
    if (inherits(obj, "Date")) return("DATE")
    if (inherits(obj, "POSIXct")) return("TIMESTAMP")
    if (inherits(obj, "integer64")) return("INT64")
    if (is.logical(obj)) return("BOOLEAN")
    if (is.integer(obj)) return("INT64")
    if (is.double(obj)) return("DOUBLE")
    if (is.raw(obj)) return("BLOB")
    if (is.character(obj) || is.factor(obj)) return("STRING")
    if (is.list(obj)) return("ANY[]")
    stop("Unsupported R type for LadybugDB.", call. = FALSE)
  })

#' @export
methods::setMethod("dbUnloadDriver", "LadybugDriver", function(drv, ...) TRUE)
