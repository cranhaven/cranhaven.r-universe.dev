#' Build a Stock Database (SD) web service url
#'
#' utility to build a url with optional query arguments
#'
#' @param service the name of the service
#' @param ... name arguments will be added as queries
#'
#' @return a complete url as a character string
#'
#' @examples
#'
#' sd_api("hi", bye = 21)
#' sd_api("odata4/StockListDWs4")
#'
#' @export
#' @importFrom httr parse_url build_url GET
sd_api <- function(service, ...) {
  url <- paste0(api_url(), "/", service)
  url <- parse_url(url)
  url$query <- list(...)
  url <- build_url(url)

  url
}
