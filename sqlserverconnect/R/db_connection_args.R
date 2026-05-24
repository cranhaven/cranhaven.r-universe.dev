#' Build SQL Server connection arguments
#'
#' @param server SQL Server hostname, IP address, or instance name
#' @param database Database name
#' @param uid Username (ignore/keep as NULL if trusted = TRUE)
#' @param pwd Password (ignore/keep as NULL if trusted = TRUE)
#' @param port Optional port number. If NULL, odbc package handles port resolution
#' @param trusted (logical) If TRUE (default), uses Windows authentication
#' @param driver ODBC driver name (default is "ODBC Driver 17 for SQL Server")
#'
#' @returns A named list of arguments suitable for a SQL Server connection
#'   string in DBI::dbConnect() or pool::dbPool(). Used internally by
#'   `db_connect()` to construct the argument list.
#'
#' @examples
#' # Build arguments using Windows authentication
#' db_connection_args(
#'   server   = "localhost",
#'   database = "master"
#' )
#'
#' # Build arguments using SQL authentication
#' db_connection_args(
#'   server   = "localhost",
#'   database = "master",
#'   uid      = "sa",
#'   pwd      = "password",
#'   trusted  = FALSE
#' )
#'
#' @export
db_connection_args <- function(server,
                               database,
                               uid = NULL,
                               pwd = NULL,
                               port = NULL,
                               trusted = TRUE,
                               driver = "ODBC Driver 17 for SQL Server") {

  server_string <- if (!is.null(port)) {
    paste0(server, ",", port)
  } else {
    server
  }

  args <- list(
    Driver   = driver,
    Server   = server_string,
    Database = database
  )

  if (trusted) {
    args$Trusted_Connection <- "Yes"
  } else {
    args$UID <- uid
    args$PWD <- pwd
  }

  return(args)
}
