#' dbInstance
#'
#' @title Indicates whether MDMAPR is connected to a database instance or not.
#'
#' @description Function to indicate if MDMAPR will be run with a database instance.
#' If MDMAPR is to be run with a database connection, update dbInstance to "Yes" and
#' if not, update dbInstance to "No". If the application is run with a database
#' connection update the dbVariables function with the correct database connection details
#' before launching app.
#'
#' @param x defines if there is a database connection or not. Must be either updated to
#' "Yes" or "No".
#'
#' @return No return value.
#'
#' @export
#'
#' @usage dbInstance(x)
#'
#' @examples
#' dbInstance("Yes")
#' dbInstance("No")
#'

dbInstance <- function(x) {
  if (x == "Yes")
  {assign("db_int", "Yes", envir = as.environment(pos))}
  else
  { assign("db_int", "No", envir = as.environment(pos))}
}


pos <- 1
