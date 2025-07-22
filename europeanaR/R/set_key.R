#' Set API key
#'
#' @description This function is a simple wrapper of the `Sys.setenv` base
#' function. It sets the value of the environmental variable `EUROPEANA_KEY`.
#' Alternatively, use .Renviron to set the key. Get and API key in the
#' following link https://pro.europeana.eu/page/get-api.
#'
#' @param api_key string with the API key
#'
#' @returns No return value, called for setting the environmental variable
#' `EUROPEANA_KEY`.
#'
#' @seealso \code{\link{setkey}}
#' @export
set_key <- function(api_key) {
  stopifnot(is.character(api_key))
  Sys.setenv(EUROPEANA_KEY = api_key)
}
