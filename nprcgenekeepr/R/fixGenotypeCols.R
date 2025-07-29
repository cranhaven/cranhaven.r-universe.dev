#' Reformat names of observed genotype columns
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
##
#' This is not a good fix. A better solution is to avoid the problem.
#' Currently qcStudbook() blindly changes all of the column names by removing
#' the underscores.
#' @return A pedigree object where column names of "firstname" and "secondname"
#' are changed to "first_name" and "second_name" respectively.
#' @param ped the pedigree information in datatable format
#' @noRd
fixGenotypeCols <- function(ped) {
  if (any(tolower(names(ped)) == "firstname")) {
    names(ped)[names(ped) == "firstname"] <- "first_name"
  }
  if (any(tolower(names(ped)) == "secondname")) {
    names(ped)[names(ped) == "secondname"] <- "second_name"
  }
  ped
}
