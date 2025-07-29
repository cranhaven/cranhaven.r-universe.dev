#' Returns TRUE if the string is a valid date.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Taken from github.com/rmsharp/rmsutilityr
#'
#' @return A logical value or \code{NA} indicating whether or not the provided
#' character vector represented a valid date string.
#'
#' @param date_str character vector with 0 or more dates
#' @param format character vector of length one having the date format
#' @param optional parameter to \code{as.Date}. Logical value indicating
#' to return NA (instead of signaling an error) if the format guessing does not
#' succeed. Defaults to FALSE.
#' @importFrom anytime anytime
#' @export
#' @examples
#' is_valid_date_str(c(
#'   "13-21-1995", "20-13-98", "5-28-1014",
#'   "1-21-15", "2-13-2098", "25-28-2014"
#' ), format = "%m-%d-%y")
is_valid_date_str <- function(date_str, format = "%d-%m-%Y %H:%M:%S",
                              optional = FALSE) {
  if (!is.character(date_str) && is.numeric(date_str)) {
      return(rep(FALSE, length(date_str)))
  }
  if (optional) {
    result <- !is.na(suppressWarnings(anytime(date_str, useR = TRUE)))
    result[result == FALSE] <- NA # nolint redundant_equals_linter
  } else {
    result <- !is.na(suppressWarnings(anytime(date_str, useR = TRUE)))
  }
  result
}
