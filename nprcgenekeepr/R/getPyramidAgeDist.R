#' Get the age distribution for the pedigree
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Forms a dataframe with columns \code{id}, \code{birth}, \code{sex},
#' and \code{age} for those animals with a status of \code{Alive} in the
#' pedigree.
#'
#' The lubridate package is used here because of the way the modern Gregorian
#' calendar is constructed, there is no straightforward arithmetic method
#' that produces a person’s age, stated according to common usage — common
#' usage meaning that a person’s age should always be an integer that
#' increases exactly on a birthday.
#'
#' @return A pedigree with \code{status} column added, which describes the
#' animal as \code{ALIVE} or \code{DECEASED} and a \code{age} column added,
#' which has the animal's age in years or \code{NA} if it cannot be calculated.
#' The \code{exit} column values have been remapped to valid dates or \code{NA}.
#'
#' @param ped dataframe with pedigree
#' @importFrom anytime anytime
#' @importFrom lubridate interval duration
#' @importFrom utils read.csv
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- getPyramidAgeDist()
getPyramidAgeDist <- function(ped = NULL) {
  if (is.null(ped)) {
    ped <- nprcgenekeepr::qcPed
    ped$age <- NULL
    ped$gen <- NULL
  }
  colNames <- c("id", "sire", "dam", "sex", "birth", "exit_date")
  names(ped) <- colNames
  ped <- ped[, colNames]
  if (!any(inherits(ped$birth, c("Date", "POSIXct", "character")))) {
    stop("birth column must be of class 'Date', 'POSIXct', or 'character'")
  } else if (class(ped$birth)[[1L]] == "character") {
    ped$birth <- suppressWarnings(anytime::anytime(ped$birth))
  } else {
    ped$birth <- suppressWarnings(as.Date(ped$birth))
  }
  ped$status[is.na(ped$exit_date)] <- "ALIVE"
  ped$status[!is.na(ped$exit_date) | is.na(ped$birth)] <- "DECEASED"
  if (!any(inherits(ped$exit_date, c("Date", "POSIXct", "character")))) {
    stop("exit_date column must be of class 'Date', 'POSIXct', or 'character'")
  } else if (class(ped$exit_date)[[1L]] == "character") {
    ped$status[ped$exit_date == "9999999999"] <- "DECEASED"
    ped$exit_date[!nzchar(ped$exit_date) | ped$exit_date == "9999999999"] <- NA
    ped$exit_date <- suppressWarnings(anytime::anytime(ped$exit_date))
  } else {
    ped$exit_date <- suppressWarnings(as.Date(ped$exit_date))
  }
  ped$age[is.na(ped$exit_date) & !is.na(ped$birth)] <-
    interval(
      start = ped$birth[is.na(ped$exit_date) &
        !is.na(ped$birth)],
      end = now()
    ) / duration(num = 1L, units = "years")
  ped$age[!is.na(ped$exit_date) & !is.na(ped$birth)] <-
    interval(
      start = ped$birth[!is.na(ped$exit_date) &
        !is.na(ped$birth)],
      end = ped$exit_date[!is.na(ped$exit_date) &
        !is.na(ped$birth)]
    ) /
      duration(num = 1L, units = "years")
  names(ped)[names(ped) == "exit_date"] <- "exit"
  ped
}
