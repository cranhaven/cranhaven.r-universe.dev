#' src - dplyr src objects
#'
#' @export
#' @name src_vegdata
#' @param path (character) path to SQLite database. by default
#' we use the function [db_path()] to get the path
#' @param ... Further args passed on to [DBI::dbConnect()]
#' @return an src object
#' @examples \dontrun{
#' # src_eurosl()
#' # src_germansl()
#' }
#' @rdname src_vegdata

 src_eurosl <- function(path = db_path("eurosl"), ...) {
  stopifnot(file.exists(path))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path, ...)
  dbplyr::src_dbi(con, auto_disconnect=TRUE)
}


#' @export
#' @rdname src_vegdata
#'
src_germansl <- function(path = db_path("germansl"), ...) {
  stopifnot(file.exists(path))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path, ...)
  dbplyr::src_dbi(con, auto_disconnect=TRUE)
}
