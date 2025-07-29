#' Sets the exit date, if there is no exit column in the table
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' Part of Pedigree Curation
#'
#'
#' @return A dataframe with an updated pedigree with exit dates specified based
#' on date information that was available.
#'
#' @param ped dataframe of pedigree and demographic information potentially
#' containing columns indicating the birth and death dates of an individual.
#' The table may also contain dates of sale (departure). Optional columns
#' are \code{birth}, \code{death}, and \code{departure}.
#' @param timeOrigin date object used by \code{as.Date} to set \code{origin}.
#' @export
#' @examples
#' library(lubridate)
#' library(nprcgenekeepr)
#' death <- mdy(paste0(
#'   sample(1:12, 10, replace = TRUE), "-",
#'   sample(1:28, 10, replace = TRUE), "-",
#'   sample(seq(0, 15, by = 3), 10, replace = TRUE) + 2000
#' ))
#' departure <- as.Date(rep(NA, 10), origin = as.Date("1970-01-01"))
#' departure[c(1, 3, 6)] <- as.Date(death[c(1, 3, 6)],
#'   origin = as.Date("1970-01-01")
#' )
#' death[c(1, 3, 5)] <- NA
#' death[6] <- death[6] + days(1)
#' ped <- data.frame(
#'   id = paste0(100 + 1:10),
#'   birth = mdy(paste0(
#'     sample(1:12, 10, replace = TRUE), "-",
#'     sample(1:28, 10, replace = TRUE), "-",
#'     sample(seq(0, 20, by = 3), 10, replace = TRUE) + 1980
#'   )),
#'   death = death,
#'   departure = departure,
#'   stringsAsFactors = FALSE
#' )
#' pedWithExit <- setExit(ped)
setExit <- function(ped, timeOrigin = as.Date("1970-01-01")) {
  headers <- tolower(names(ped))
  if (nrow(ped) == 0L) {
    return(ped)
  }
  if (("birth" %in% headers) && !("exit" %in% headers)) {
    if (any("death" %in% headers) && any("departure" %in% headers)) {
      # mapply simplifies results by default
      # mapply would return a list, but simplification coerces this to a vector
      # consequently, the simplification also coerces Date columns to Numeric
      # as.Date("1970-01-01") is used to counter this and maintain Dates
      # properly
      ped$exit <- as.Date(mapply(chooseDate, ped$death, ped$departure),
        origin = timeOrigin
      )
    } else if ("death" %in% headers) {
      ped$exit <- ped$death
    } else if ("departure" %in% headers) {
      ped$exit <- ped$departure
    } else {
      ped$exit <- as.Date(NA, origin = timeOrigin)
    }
  }
  return(ped)
}
