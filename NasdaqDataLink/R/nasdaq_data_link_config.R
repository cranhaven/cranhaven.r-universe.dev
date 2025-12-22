#' Query or set Nasdaq Data Link API key
#' @param api_key Optionally passed parameter to set Nasdaq Data Link \code{api_key}.
#' @return Returns invisibly the currently set \code{api_key}.
#' @examples \dontrun{
#' NasdaqDataLink.api_key('foobar')
#' }
#' @export
NasdaqDataLink.api_key <- function(api_key) {
  if (!missing(api_key)) {
    options(NasdaqDataLink.api_key = api_key)
  }
  invisible(getOption("NasdaqDataLink.api_key"))
}

#' Query or set Base URL for White-labeled sites
#' @param base_url Optionally passed parameter to set Nasdaq Data Link \code{base_url}.
#' @return Returns invisibly the currently set \code{base_url}.
#' @examples \dontrun{
#' NasdaqDataLink.base_url('http://localhost')
#' }
#' @export
NasdaqDataLink.base_url <- function(base_url) {
  if (!missing(base_url)) {
    options(NasdaqDataLink.base_url = paste(gsub('\\/$', '', base_url), "/api/v3", sep = ""))
  }
  invisible(getOption("NasdaqDataLink.base_url", "https://data.nasdaq.com/api/v3"))
}

#' Query or set Nasdaq Data Link API token
#'
#' Deprecated. Alias of \code{\link{NasdaqDataLink.api_key}}
#'
#' @param auth_token Optionally passed parameter to set Nasdaq Data Link \code{auth_token}.
#' @return Returns invisibly the currently set \code{auth_token}.
#' @seealso \code{\link{NasdaqDataLink.api_key}}
#' @examples \dontrun{
#' NasdaqDataLink.auth('foobar')
#' }
#' @export
NasdaqDataLink.auth <- function(auth_token) {
  .Deprecated("NasdaqDataLink.api_key")
  NasdaqDataLink.api_key(auth_token)
}

NasdaqDataLink.api_version <- function(api_version) {
  if (!missing(api_version)) {
    options(NasdaqDataLink.api_version = api_version)
  }
  invisible(getOption("NasdaqDataLink.api_version"))
}
