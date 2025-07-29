#' Forms a dataframe with Id, Sex, and current Age given a list of Ids and a
#' pedigree
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Dataframe with Id, Sex, and Current Age
#'
#' @param ids character vector of animal Ids
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' data("qcBreeders")
#' data("qcPed")
#' df <- addSexAndAgeToGroup(ids = qcBreeders, ped = qcPed)
#' head(df)
addSexAndAgeToGroup <- function(ids, ped) {
  group <- data.frame(
    ids,
    sex = sapply(ids, function(id) {
      ped$sex[ped$id == id]
    }),
    age = vapply(ids, function(id) {
      getCurrentAge(ped$birth[ped$id == id])
    }, numeric(1L)),
    stringsAsFactors = FALSE
  )
  group
}
