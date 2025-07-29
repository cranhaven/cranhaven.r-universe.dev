#' obfucateDate adds a random number of days bounded by plus and minus max delta
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Get the base_date add a random number of days taken from a uniform
#' distribution bounded by -max_delta and max_delta.
#' Insure the resulting date is as least as large as the min_date.
#'
#' @return A vector of dates that have be obfuscated.
#'
#' @param baseDate list of Date objects with dates to be obfuscated
#' @param minDate list object of Date objects that has the lower bound of
#' resulting obfuscated dates
#' @param maxDelta integer vector that is used to create min and max arguments
#' to \code{runif} (\code{runif(n, min = 0, max = 1)})
#' @importFrom lubridate ddays
#' @importFrom stats runif
#' @export
#' @examples
#' library(nprcgenekeepr)
#' someDates <- rep(
#'   as.Date(c("2009-2-16", "2016-2-16"), format = "%Y-%m-%d"),
#'   10
#' )
#' minBirthDate <- rep(as.Date("2009-2-16", format = "%Y-%m-%d"), 20)
#' obfuscateDate(someDates, minBirthDate, 30L)
obfuscateDate <- function(baseDate, minDate, maxDelta = 30L) {
  if (length(maxDelta) == 1L) {
    maxDelta <- rep(maxDelta, length(baseDate))
  }
  if (length(baseDate) != length(maxDelta)) {
    stop("Length of minDate must be 1 or the same as baseDate.")
  }
  if (missing(minDate)) {
    minDate <- baseDate
    for (i in seq_along(baseDate)) {
      minDate[[i]] <- as.Date(baseDate[[i]] - ddays(maxDelta[[i]]))
    }
  }
  if (length(baseDate) != length(minDate)) {
    stop("Length of baseDate and minDate must be the same.")
  }

  obfuscatedDates <- baseDate
  for (i in seq_along(baseDate)) {
    if (is.na(baseDate[[i]])) {
      obfuscatedDate <- NA
    } else {
      repeat {
        obfuscatedDate <- as.Date(baseDate[[i]] + ddays(runif(
          1L, -maxDelta[i],
          maxDelta[i]
        )))
        if (obfuscatedDate >= minDate[[i]]) {
          break
        }
      }
    }
    obfuscatedDates[[i]] <- obfuscatedDate
  }
  obfuscatedDates
}
