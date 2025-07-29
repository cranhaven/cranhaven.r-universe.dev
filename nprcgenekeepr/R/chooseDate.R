#' Choose date based on \code{earlier} flag.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Pedigree Curation
#'
#' Given two dates, one is selected to be returned based on whether
#' it occurred earlier or later than the other. \code{NAs} are ignored if
#' possible.
#'
#' @return \code{Date} vector of chosen dates or \code{NA} where neither
#' is provided
#'
#' @param d1 \code{Date} vector with the first of two dates to compare.
#' @param d2 \code{Date} vector with the second of two dates to compare.
#' @param earlier logical variable with \code{TRUE} if the earlier of the two
#' dates is to be returned, otherwise the later is returned. Default is
#' \code{TRUE}.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' someDates <- lubridate::mdy(paste0(
#'   sample(1:12, 2, replace = TRUE), "-",
#'   sample(1:28, 2, replace = TRUE), "-",
#'   sample(seq(0, 15, by = 3), 2,
#'     replace = TRUE
#'   ) + 2000
#' ))
#' someDates
#' chooseDate(someDates[1], someDates[2], earlier = TRUE)
#' chooseDate(someDates[1], someDates[2], earlier = FALSE)
chooseDate <- function(d1, d2, earlier = TRUE) {
  if (is.na(d1)) {
    return(d2)
  } else if (is.na(d2)) {
    return(d1)
  } else if ((d1 < d2) && earlier) {
    return(d1)
  } else if ((d1 > d2) && !earlier) {
    return(d1)
  } else {
    return(d2)
  }
}
