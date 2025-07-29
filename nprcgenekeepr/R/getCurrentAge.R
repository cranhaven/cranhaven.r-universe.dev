#' Age in years using the provided birthdate.
#'
#' Assumes current date for calculating age.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Age in years using the provided birthdate.
#'
#' @param birth birth date(s)
#' @importFrom lubridate duration interval today
#' @export
#' @examples
#' library(nprcgenekeepr)
#' age <- getCurrentAge(birth = as.Date("06/02/2000", format = "%m/%d/%Y"))
getCurrentAge <- function(birth) {
  as.numeric(interval(start = birth, end = today()) /
    duration(num = 1L, units = "years"))
}
