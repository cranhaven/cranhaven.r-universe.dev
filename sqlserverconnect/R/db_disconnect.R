#' Disconnect from a SQL Server connection or pool
#'
#' @param conn A DBI connection or a pool connection object
#' @param quiet (logical) if TRUE, suppresses messages
#'
#' @return TRUE (invisibly) if disconnected, FALSE otherwise
#'
#' @examples
#' \donttest{
#' # Establish a connection
#' conn <- db_connect(
#'   server   = "localhost",
#'   database = "master",
#'   quiet    = TRUE
#' )
#'
#' # Disconnect when finished
#' db_disconnect(conn)
#'
#' # Disconnecting a NULL connection
#' db_disconnect(NULL)
#' }
#'
#' @export
db_disconnect <- function(conn, quiet = FALSE) {
  if (is.null(conn)) {
    if (!quiet) cli::cli_alert_info("No connection to disconnect.")
    invisible(FALSE)
  } else if(inherits(conn, 'Pool') && DBI::dbIsValid(conn)) {
    pool::poolClose(conn)
    if (!quiet) cli::cli_alert_success("Pooled connection closed.")
    invisible(TRUE)
  } else if(inherits(conn, 'Pool') && !DBI::dbIsValid(conn)) {
    if (!quiet) cli::cli_alert_info("Pooled connection was already closed.")
    invisible(TRUE)
  } else if(inherits(conn, "DBIConnection") && DBI::dbIsValid(conn)) {
    try(DBI::dbDisconnect(conn), silent = TRUE)
    if (!quiet) cli::cli_alert_success("Database connection closed.")
    invisible(TRUE)
  } else if(inherits(conn, "DBIConnection") && !DBI::dbIsValid(conn)) {
    if (!quiet) cli::cli_alert_info("Database connection was already closed.")
    invisible(TRUE)
  } else {
    if(!quiet) cli::cli_alert_warning("Object is not a DBI connection or pool.")
    invisible(FALSE)
  }






}
