#' Retrieves Point In Time Data from the Nasdaq Data Link PIT endpoint.
#' From start to end will also show all rows that were visible at any point between two specified points in time, including start, but excluding end.
#'
#' @details Set your \code{api_key} with \code{NasdaqDataLink.api_key} function. For instructions on finding your api key go to \url{https://data.nasdaq.com/account/profile}
#'
#' @param datatable_code Datatable code on Nasdaq Data Link specified as a string.
#' @param start_date Lower date as a string.
#' @param end_date Upper date as a string.
#' @param paginate When set to TRUE, fetches up to 1,000,000 rows of data
#' @param ... Additional named values that are interpreted as Nasdaq Data Link API parameters.
#' @return Returns a data.frame.
#' @seealso \code{\link{NasdaqDataLink.api_key}}
#' @examples \dontrun{
#' NasdaqDataLink.pit.fromto('RSM/MSB', '2020-06-11', '2020-06-12', paginate=TRUE)
#' }
#' @export
NasdaqDataLink.pit.fromto <- function(datatable_code, start_date, end_date, paginate = FALSE, ...) {
  path <- paste0("pit/", datatable_code, '/from/', start_date, '/to/', end_date)
  nasdaq_data_link.datatable.perform(path, paginate, list(...))
}

#' Retrieves Point In Time Data from the Nasdaq Data Link PIT endpoint.
#' Between start and end will show all rows that were visible at any point between two specified points in time. It works inclusively, a row visible exactly at start or exactly at end will be shown too.
#'
#' @details Set your \code{api_key} with \code{NasdaqDataLink.api_key} function. For instructions on finding your api key go to \url{https://data.nasdaq.com/account/profile}
#'
#' @param datatable_code Datatable code on Nasdaq Data Link specified as a string.
#' @param start_date Lower date as a string.
#' @param end_date Upper date as a string.
#' @param paginate When set to TRUE, fetches up to 1,000,000 rows of data
#' @param ... Additional named values that are interpreted as Nasdaq Data Link API parameters.
#' @return Returns a data.frame.
#' @seealso \code{\link{NasdaqDataLink.api_key}}
#' @examples \dontrun{
#' NasdaqDataLink.pit.between('RSM/MSB', '2020-06-11', '2020-06-12', paginate=TRUE)
#' }
#' @export
NasdaqDataLink.pit.between <- function(datatable_code, start_date, end_date, paginate = FALSE, ...) {
  path <- paste0("pit/", datatable_code, '/between/', start_date, '/', end_date)
  nasdaq_data_link.datatable.perform(path, paginate, list(...))
}

#' Retrieves Point In Time Data from the Nasdaq Data Link PIT endpoint.
#' As of date is used to see the table as it was at a specific point in time in the past.
#'
#' @details Set your \code{api_key} with \code{Nasdaq Data Link.api_key} function. For instructions on finding your api key go to \url{https://data.nasdaq.com/account/profile}
#'
#' @param datatable_code Datatable code on Nasdaq Data Link specified as a string.
#' @param start_date Lower date as a string.
#' @param paginate When set to TRUE, fetches up to 1,000,000 rows of data
#' @param ... Additional named values that are interpreted as Nasdaq Data Link API parameters.
#' @return Returns a data.frame.
#' @seealso \code{\link{NasdaqDataLink.api_key}}
#' @examples \dontrun{
#' NasdaqDataLink.pit.asofdate('RSM/MSB', '2020-06-11', paginate=TRUE)
#' }
#' @export
NasdaqDataLink.pit.asofdate <- function(datatable_code, start_date, paginate = FALSE, ...) {
  path <- paste0("pit/", datatable_code, '/asofdate/', start_date)
  nasdaq_data_link.datatable.perform(path, paginate, list(...))
}
