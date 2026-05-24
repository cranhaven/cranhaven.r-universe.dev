#' Connect to a SQL Server database
#'
#' @inheritParams db_connection_args
#' @param pool (logical) if TRUE, returns a pooled connection
#' @param quiet (logical) if TRUE, suppresses messages
#' @param ... Additional arguments passed to DBI::dbConnect or pool::dbPool
#'
#' @returns A DBI connection or a pool object
#'
#' @examples
#' \donttest{
#' # Connect to a SQL Server database
#' conn <- db_connect(
#'   server   = "localhost",
#'   database = "master",
#'   quiet    = TRUE
#' )
#'
#' # Run a simple query
#' DBI::dbGetQuery(conn, "SELECT name FROM sys.databases")
#'
#' # Disconnect when finished
#' db_disconnect(conn)
#' }
#'
#' @export
db_connect <- function(server,
                       database,
                       uid = NULL,
                       pwd = NULL,
                       port = NULL,
                       trusted = TRUE,
                       driver = "ODBC Driver 17 for SQL Server",
                       pool = FALSE,
                       quiet = FALSE,
                       ...) {

  args <- db_connection_args(
    server   = server,
    database = database,
    uid      = uid,
    pwd      = pwd,
    port     = port,
    trusted  = trusted,
    driver   = driver
  )

  if (pool) {
    conn <- do.call(
      pool::dbPool,
      c(list(drv = odbc::odbc()), args, list(...))
    )
    if (!quiet) cli::cli_alert_success("Pooled connection established.")
    conn
  } else {
    conn <- do.call(
      DBI::dbConnect,
      c(list(drv = odbc::odbc()), args, list(...))
    )
    if (!quiet) cli::cli_alert_success("Database connection established.")
    conn
  }
}
