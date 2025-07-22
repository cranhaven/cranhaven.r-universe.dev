#' Get API key
#'
#' @description This function is a simple wrapper of the `Sys.getenv` base
#' function. It gets the value of the environmental variable `EUROPEANA_KEY`.
#' @returns character with the API key stored as environmental variable
#' @seealso \code{\link{set_key}}
#' @export
get_key <- function() {
  api_key <- Sys.getenv("EUROPEANA_KEY")
  stopifnot(`Provide an API key with set_key()` = api_key != "")
  api_key
}
