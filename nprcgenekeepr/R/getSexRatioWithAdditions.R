#' getSexRatioWithAdditions returns the sex ratio of a group.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Adding males and females to the ratio calculation is possible, but the
#' default behavior is to simply return the sex ratio of the group.
#' This is a helper routine for the main one \code{calculateSexRatio}.
#' @return Numeric value representing the sex ratio of the proposed group.
#' @param ids character vector of animal Ids
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @param additionalMales Integer value of males to add to those within the
#' group when calculating the ratio. Ignored if calculated ratio is 0 or Inf.
#' Default is 0.
#' @param additionalFemales Integer value of females to add to those within the
#' group when calculating the ratio. Ignored if calculated ratio is 0 or Inf.
#' Default is 0.
#' @noRd
getSexRatioWithAdditions <- function(ids, ped, additionalMales,
                                     additionalFemales) {
  (length(ped$sex[ped$id %in% ids & ped$sex != "M"]) + additionalFemales) /
    (length(ped$sex[ped$id %in% ids & ped$sex == "M"]) + additionalMales)
}
