#' Check if MME Calculator API response indicates transient error
#'
#' @param resp A httr2 response object.
#'
#' @returns
#' A logical value indicating whether there was a rate limit error (429) and
#'     no API calls remaining.
#'
#' @keywords internal
#'
mmequiv_is_transient <- function(resp) {
  # Extract RateLimit header
  rate_limit_header <- httr2::resp_header(resp, "RateLimit")

  # Split header parts to extract "remaining=x"
  parts <- strsplit(rate_limit_header, ",\\s*")[[1]]
  remaining_part <- parts[grep("remaining=", parts)]

  # Extract the numeric value after 'remaining='
  remaining <- sub("remaining=(\\d+).*", "\\1", remaining_part)

  httr2::resp_status(resp) == 429 &&
    remaining == "0"
}

#' Get time remaining from Retry-After header
#'
#' @param resp A httr2 response object.
#'
#' @returns
#' A numeric value indicating the number of seconds remaining. May error if
#' the response doesn't contain a valid Retry-After header.
#'
#' @keywords internal
#'
mmequiv_after <- function(resp) {
  time <- as.numeric(httr2::resp_header(resp, "Retry-After"))
  time - unclass(Sys.time())
}

#' Automatically retry a request on failure with `mmequiv` package details
#'
#' @param req A httr2 request object.
#'
#' @returns
#' A modified HTTP (`httr2`) request.
#'
#' @keywords internal
#'
mmequiv_req_retry <- function(req, ...) {
  httr2::req_retry(
    req = req,
    is_transient = mmequiv_is_transient,
    after = mmequiv_after,
    max_seconds = 900
  )
}
