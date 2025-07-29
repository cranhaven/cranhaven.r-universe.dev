#' removeEarlyDates removes dates before a specified year
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Dates before a specified year are set to NA. This is often used for dates
#' formed from malformed character representations such as a date in
#' %m-%d-%Y format being read by %Y-%m-%d format
#'
#' NA values are ignored and not changed.
#'
#' @return A vector of dates after the year indicated by the numeric value of
#' \code{firstYear}.
#'
#' @param dates vector of dates
#' @param firstYear integer value of first (earliest) year in the allowed
#' date range.
#' @importFrom lubridate year
#' @export
#' @examples
#' dates <- structure(c(
#'   12361, 14400, 15413, NA, 11189, NA, 13224, 10971,
#'   -432000, 13262
#' ), class = "Date")
#' cleanedDates <- removeEarlyDates(dates, firstYear = 1000)
#' dates
#' cleanedDates
removeEarlyDates <- function(dates, firstYear) {
  dates[year(dates) < firstYear & !is.na(dates)] <- NA
  dates
}
