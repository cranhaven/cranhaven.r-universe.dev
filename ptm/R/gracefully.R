## --------------- gracefully.R ---------------- ##
#                                                 #
#   gracefully_fail                               #
#                                                 #
## --------------------------------------------- ##

## ----------------------------------------------------------------- ##
#           gracefully_fail <- function(call, timeout, ...)           #
## ----------------------------------------------------------------- ##
#' Check that Internet Resource Work Properly and Fail Gracefully When Not
#' @description Checks that internet resource works properly and fail gracefully when not.
#' @usage gracefully_fail(call, timeout = 10,  ...)
#' @param call url of the resource.
#' @param timeout set maximum request time in seconds.
#' @param ... further named parameters, such as query, headers, etc.
#' @details To be used as an ancillary function.
#' @return The response object or NULL when the server does not respond properly.
#' @author thefactmachine
#' @examples gracefully_fail("http://httpbin.org/delay/2")
#' @references https://gist.github.com/thefactmachine/18279b7796c0836d9188
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr timeout
#' @importFrom httr message_for_status
#' @importFrom curl has_internet
#' @export

gracefully_fail <- function(call, timeout = 10, ...){

  try_GET <- function(x, timeout, ...){
    tryCatch(
      GET(url = x, timeout(timeout), ...),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }

  is_response <- function(x) {
    class(x) == "response"
  }

  # First check internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }
  # Then try for timeout problems
  resp <- try_GET(call, timeout, ...)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # Then stop if status > 400
  if (httr::http_error(resp)) {
    message_for_status(resp)
    return(invisible(NULL))
  }

  # If you are using rvest, you can easily read_html in the response
  # xml2::read_html(resp)

  httr::content(resp, 'text')
}
