#' Get the maximum age of live animals in the pedigree.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Numeric value representing the maximum age of animals in the
#' pedigree.
#'
#' @param ped dataframe with pedigree
#' @export
#' @examples
#' library(nprcgenekeepr)
#' examplePedigree <- nprcgenekeepr::examplePedigree
#' ped <- qcStudbook(examplePedigree,
#'   minParentAge = 2,
#'   reportChanges = FALSE,
#'   reportErrors = FALSE
#' )
#' getPedMaxAge(ped)
getPedMaxAge <- function(ped) {
  max(ped$age, na.rm = TRUE)
}
