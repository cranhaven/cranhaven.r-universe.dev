#' Assert Existence of URL
#'
#' @description Assert that a Uniform Resource Locator (URL) is complete and valid.
#'   Requires that the \pkg{httr} package is available.
#'
#' @param url 'character' string.
#'   URL
#' @param ...
#'   Other arguments passed to the [`httr::HEAD`] and [`httr::GET`] functions.
#'
#' @return Returns `url` invisibly.
#'   A `NULL` value is returned if the assertion fails for any reason.
#'
#' @source Code adapted from
#'   [Stack Overflow](https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r),
#'   accessed on 2023-12-11 and authored by Bob Rudis.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' \donttest{
#' assert_url("https://www.usgs.gov/")
#' }

assert_url <- function(url, ...) {

  # check arguments
  checkmate::assert_string(url)

  # check packages
  check_package(pkg = "httr", msg = "URL assertions")

  # check internet connection
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }

  f <- safely(httr::HEAD)
  res <- f(url, ...)

  if (is.null(res$result) || ((httr::status_code(res$result) %/% 200) != 1)) {
    f <- safely(httr::GET)
    res <- f(url, ...)
    if (is.null(res$result)) {
      sprintf("Assertion on '%s' failed: hard error with no response.", url) |>
        message()
      return(invisible(NULL))
    }

    if (((httr::status_code(res$result) %/% 200) != 1)) {
      sprintf("Assertion on '%s' failed: responded but without an HTTP status code in the 200-299 range.", url) |>
        message()
      return(invisible(NULL))
    }
  }

  invisible(url)
}

safely <- function(f) {
  checkmate::assert_function(f)
  function(...) {
    capture_error(
      f(...)
    )
  }
}

capture_error <- function(code) {
  tryCatch(
    expr = {
      list(result = code, error = NULL)
    },
    error = function(e) {
      list(result = NULL, error = e)
    },
    interrupt = function(e) {
      stop("Terminated by user", call. = FALSE)
    }
  )
}
