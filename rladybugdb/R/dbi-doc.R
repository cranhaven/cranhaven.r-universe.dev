#' LadybugDB DBI methods
#'
#' LadybugDB implements the standard DBI lifecycle, query, binding, fetch,
#' transaction, quoting, and table-metadata generics. LadybugDB's native schema
#' distinguishes node and relationship tables, so relational helpers outside
#' this documented subset are intentionally left to graph-specific Cypher and
#' the `lb_*` API. SQL `SELECT`, `dbWriteTable()`, and `dbAppendTable()` are not
#' translated. `dbExecute()` returns zero after successful DDL/DML because the
#' LadybugDB 0.20.4 C result summary does not expose a stable affected-row
#' count. Applicable driver behavior is exercised with a focused DBItest
#' subset.
#'
#' @return The return value depends on the DBI method:
#' * `dbConnect()` returns a `LadybugConnection` S4 object, inheriting from
#'   `DBIConnection`, that owns an open database and native connection.
#' * `dbSendQuery()` and `dbSendStatement()` return a `LadybugResult` S4 object,
#'   inheriting from `DBIResult`, representing an executed statement or a
#'   parameterized statement awaiting `dbBind()`.
#' * `dbBind()` returns the supplied `LadybugResult` invisibly after executing
#'   the statement with the supplied parameters and resetting its fetch cursor.
#' * `dbFetch()` and `dbGetQuery()` return a `data.frame` with one column per
#'   returned expression and one row per query record. `dbFetch()` advances the
#'   cursor; `dbGetQuery()` fetches all rows and clears its result.
#' * `dbExecute()` and `dbGetRowsAffected()` return the numeric scalar `0`.
#'   The engine does not expose an affected-row count, so this value does not
#'   indicate how many rows were changed.
#' * `dbGetRowCount()` returns a numeric scalar counting rows fetched since
#'   the result was created or most recently bound.
#' * `dbColumnInfo()` returns a `data.frame` with character columns `name` and
#'   `type`, giving each result column's name and LadybugDB type.
#' * `dbGetInfo()` returns a named list. For a driver, `driver.version` and
#'   `client.version` are character strings identifying the package and engine
#'   versions. For a connection, `dbname`, `bigint`, and `serverVersion` are
#'   character strings describing its path, integer conversion policy, and
#'   engine version; `read.only` and `valid` are logical scalars. For a result,
#'   `statement` is the Cypher string, `row.count` is the numeric fetched-row
#'   count, and `completed` is a logical scalar. An unbound result also has
#'   `pending = TRUE`; an executed, valid result also includes the metadata
#'   described by [lb_result_info()].
#' * `dbIsValid()`, `dbHasCompleted()`, and `dbIsReadOnly()` return a logical
#'   scalar indicating whether the object is usable, fetching is complete, or
#'   the database was opened read-only, respectively. Drivers are always valid.
#' * `dbExistsTable()` returns a logical vector indicating whether each
#'   supplied table name exists.
#' * `dbListTables()` and `dbListFields()` return character vectors of table
#'   names and field names, respectively; an empty vector means none were found.
#' * `dbDataType()` returns a character string naming the LadybugDB type for
#'   an R vector, or a named character vector of types for data-frame columns.
#' * `dbQuoteIdentifier()` and `dbQuoteString()` return `DBI::SQL` objects
#'   containing quoted identifiers or string literals, with the same length
#'   as the input character vector.
#' * `dbDisconnect()`, `dbClearResult()`, `dbBegin()`, `dbCommit()`,
#'   `dbRollback()`, and `dbUnloadDriver()` return the logical scalar `TRUE`
#'   after successfully releasing resources or completing transaction control.
#' * `show()` returns invisible `NULL`, called for the side effect of
#'   displaying a summary of the driver, connection, or result.
#'
#' @name LadybugDB-DBI-methods
#' @aliases dbBegin,LadybugConnection-method
#' @aliases dbBind,LadybugResult-method
#' @aliases dbClearResult,LadybugResult-method
#' @aliases dbColumnInfo,LadybugResult-method
#' @aliases dbCommit,LadybugConnection-method
#' @aliases dbConnect,LadybugDriver-method
#' @aliases dbDataType,LadybugDriver-method
#' @aliases dbDisconnect,LadybugConnection-method
#' @aliases dbExecute,LadybugConnection,character-method
#' @aliases dbExistsTable,LadybugConnection,character-method
#' @aliases dbFetch,LadybugResult-method
#' @aliases dbGetInfo,LadybugConnection-method
#' @aliases dbGetInfo,LadybugDriver-method
#' @aliases dbGetInfo,LadybugResult-method
#' @aliases dbGetQuery,LadybugConnection,character-method
#' @aliases dbGetRowCount,LadybugResult-method
#' @aliases dbGetRowsAffected,LadybugResult-method
#' @aliases dbHasCompleted,LadybugResult-method
#' @aliases dbIsReadOnly,LadybugConnection-method
#' @aliases dbIsValid,LadybugConnection-method
#' @aliases dbIsValid,LadybugDriver-method
#' @aliases dbIsValid,LadybugResult-method
#' @aliases dbListFields,LadybugConnection,character-method
#' @aliases dbListTables,LadybugConnection-method
#' @aliases dbQuoteIdentifier,LadybugConnection,character-method
#' @aliases dbQuoteString,LadybugConnection,character-method
#' @aliases dbRollback,LadybugConnection-method
#' @aliases dbSendQuery,LadybugConnection,character-method
#' @aliases dbSendStatement,LadybugConnection,character-method
#' @aliases dbUnloadDriver,LadybugDriver-method
#' @aliases show,LadybugConnection-method
#' @aliases show,LadybugDriver-method
#' @aliases show,LadybugResult-method
#' @seealso [Ladybug()], [DBI::DBI-package]
NULL
