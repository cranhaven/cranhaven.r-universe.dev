#' Calculates the sex ratio (number of non-males / number of males) given
#' animal Ids and their pedigree
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @description The Males are counted when the \code{ped$sex} value is
#' \code{"M"}.
#' Females are counted when the \code{ped$sex} value is not
#' \code{"M"}. This means animals with ambiguous sex are counted with the
#' females.
#'
#' @return Numeric value of sex ratio of the animals provided.
#'
#' @param ids character vector of animal Ids
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @param additionalMales Integer value of males to add to those within the
#' group when calculating the ratio. Ignored if calculated ratio is 0 or Inf.
#' Default is 0.
#' @param additionalFemales Integer value of females to add to those within the
#' group when calculating the ratio. Ignored if calculated ratio is 0 or Inf.
#' Default is 0.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' data("qcBreeders")
#' data("pedWithGenotype")
#' available <- c(
#'   "JGPN6K", "8KM1MP", "I9TQ0T", "Q0RGP7", "VFS0XB", "CQC133",
#'   "2KULR3", "HOYW0S", "FHV13N", "OUM6QF", "6Z7MD9", "CFPEEU",
#'   "HLI95R", "RI0O7F", "7M51X5", "DR5GXB", "170ZTZ", "C1ICXL"
#' )
#' nonMales <- c(
#'   "JGPN6K", "8KM1MP", "I9TQ0T", "Q0RGP7", "CQC133",
#'   "2KULR3", "HOYW0S", "FHV13N", "OUM6QF", "6Z7MD9", "CFPEEU",
#'   "HLI95R", "RI0O7F", "7M51X5", "DR5GXB", "170ZTZ", "C1ICXL"
#' )
#' male <- "VFS0XB"
#' calculateSexRatio(ids = male, ped = pedWithGenotype)
#' calculateSexRatio(ids = nonMales, ped = pedWithGenotype)
#' calculateSexRatio(ids = available, ped = pedWithGenotype)
#' calculateSexRatio(
#'   ids = available, ped = pedWithGenotype,
#'   additionalMales = 1L
#' )
#' calculateSexRatio(
#'   ids = available, ped = pedWithGenotype,
#'   additionalFemales = 1L
#' )
#' calculateSexRatio(
#'   ids = available, ped = pedWithGenotype,
#'   additionalMales = 1, additionalFemales = 1L
#' )
#' calculateSexRatio(
#'   ids = nonMales, ped = pedWithGenotype,
#'   additionalMales = 1, additionalFemales = 0L
#' )
#' calculateSexRatio(
#'   ids = character(0), ped = pedWithGenotype,
#'   additionalMales = 1, additionalFemales = 0L
#' )
calculateSexRatio <- function(ids, ped, additionalMales = 0L,
                              additionalFemales = 0L) {
  if (length(ids) == 0L) {
    if (additionalFemales > 0L) {
      if (additionalMales == 0L) {
        ratio <- Inf
      } else if (additionalMales > 0L) {
        ratio <- getSexRatioWithAdditions(
          ids, ped, additionalMales,
          additionalFemales
        )
      }
    } else if (additionalFemales == 0L) {
      if (additionalMales == 0L) {
        ratio <- NA
      } else {
        ratio <- 0.0
      }
    }
  } else if (length(ped$sex[ped$id %in% ids & ped$sex == "M"]) == 0L) {#no males
    if (additionalMales > 0L) {
      ratio <- getSexRatioWithAdditions(
        ids, ped, additionalMales,
        additionalFemales
      )
    } else {
      ratio <- Inf
    }
  } else {
    ratio <- getSexRatioWithAdditions(
      ids, ped, additionalMales,
      additionalFemales
    )
  }

  ratio
}
