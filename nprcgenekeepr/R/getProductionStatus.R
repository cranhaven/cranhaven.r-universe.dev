#' Get production status of group
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return \code{production} -- Ratio of the number of births that live >30
#' days to the number of females >= 3 years of age.
#'
#' @details Description of how Production and Production Status (color) is
#' calculated.
#' \enumerate{
#' \item  The Production Status is calculated on September 09, 2019,
#'        Births = count of all animals in group born since January 1,
#'        2017 through December 31, 2018, that lived at least 30 days.
#' \item  Dams = count of all females in group that have a birth date on or
#'        prior to September 09, 2016.
#' \item  Production = Births / Dams
#' \item  Production Status (color)
#'     \enumerate{
#'     \item  Shelter and pens
#'         \enumerate{
#'         \item   Production < 0.6; Red
#'         \item   Production >= 0.6 and Production <= 0.63; Yellow
#'         \item   Production > 0.63; Green
#'     }
#'     \item  Corrals
#'         \enumerate{
#'         \item   Production < 0.5; Red
#'         \item   Production >= 0.5 and Production <= 0.53; Yellow
#'         \item   Production > 0.53; Green
#'         }
#'     }
#'  }
#'
#' This code may need to be modified to allow the user to supply a list
#' of IDs to include as group members. Currently each animal in the
#' provided pedigree (\code{ped}) is considered to be a member of the
#' group.
#' @param ped Dataframe that is the `Pedigree`. It contains pedigree
#' information. The \code{id}, \code{dam}, \code{sex} and \code{age}
#' (in years) columns are required.
#' @param minParentAge Numeric values to set the minimum age in years for
#' an animal to have an offspring. Defaults to 2 years. The check is not
#' performed for animals with missing birth dates.
#' @param maxOffspringAge Numeric values to set the maximum age in years for
#' an animal to be counted as birth in calculation of production status
#' ratio.
#' @param housing character vector of length 1 having the housing type, which
#' is either \emph{"shelter_pens"} or \emph{"corral"}.
#' @param currentDate Date to be used for calculating age. Defaults to
#'        \code{Sys.Date()}.
#' @importFrom lubridate as.duration ddays interval mdy year
#' @noRd
getProductionStatus <- function(ped, minParentAge = 3L, maxOffspringAge = NULL,
                                housing = "shelter_pens",
                                currentDate = Sys.Date()) {
  expectedCols <- c("id", "dam", "sex", "age")
  if (!all(expectedCols %in%
    names(ped))) {
    missingCol <- expectedCols[!expectedCols %in% names(ped)]
    stop("ped is missing: ", missingCol)
  }
  nDam <- nrow(ped[ped$sex == "F" & ped$age >= minParentAge, ])
  if (is.null(maxOffspringAge)) {
    # nolint start: nonportable_path_linter
    maxOffspringAge <- mdy(paste0("1/1/", year(currentDate) - 2L))
    startDate <- mdy(paste0("1/1/", year(currentDate) - 2L))
    endDate <- mdy(paste0("12/31/", year(currentDate) - 1L))
    # nolint end:
  }
  ped <- ped[!(is.na(ped$birth) | is.na(ped$exit)), ]
  nOffspring <-
    nrow(ped[ped$birth >= startDate &
      ped$birth <= endDate &
      as.numeric(as.duration(
        interval(ped$birth, ped$exit)
      ) / ddays(1L)) >= 30L, ])
  if (nDam > 0L) {
    production <- nOffspring / nDam
  } else {
    production <- NA
  }

  if (housing == "shelter_pens") {
    if (is.na(production) || production > 0.63) {
      color <- "green"
      colorIndex <- 3L
    } else if (production < 0.6) {
      color <- "red"
      colorIndex <- 1L
    } else if (production >= 0.6 && production <= 0.63) {
      color <- "yellow"
      colorIndex <- 2L
    }
  } else if (housing == "corral") {
    if (is.na(production) || production > 0.53) {
      color <- "green"
      colorIndex <- 3L
    } else if (production < 0.5) {
      color <- "red"
      colorIndex <- 1L
    } else if (production >= 0.5 && production <= 0.53) {
      color <- "yellow"
      colorIndex <- 2L
    }
  } else {
    stop(
      "Undefined housing type in getProduction status is: ",
      housing
    )
  }
  list(production = production, color = color, colorIndex = colorIndex)
}
