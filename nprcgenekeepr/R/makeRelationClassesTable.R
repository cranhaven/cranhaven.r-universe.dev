#' Make relation classes table from \code{kin} dataframe.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' From Relations
#'
#' @return A data.frame with the number of instances of following relationship
#' classes: Parent-Offspring, Full-Siblings, Half-Siblings,
#' Grandparent-Grandchild, Full-Cousins, Cousin - Other, Full-Avuncular,
#' Avuncular - Other, Other, and No Relation.
#'
#' @param kin a dataframe with columns \code{id1}, \code{id2}, \code{kinship},
#' and \code{relation}. It is a long-form table of pairwise kinships, with
#' relationship categories included for each pair.
#' @export
#' @examples
#' library(nprcgenekeepr)
#' suppressMessages(library(dplyr))
#'
#' qcPed <- nprcgenekeepr::qcPed
#' qcPed <- qcPed[1:50, ] # Comment out for full example
#' bkmat <- kinship(qcPed$id, qcPed$sire, qcPed$dam, qcPed$gen,
#'   sparse = FALSE
#' )
#' kin <- convertRelationships(bkmat, qcPed)
#' relClasses <- makeRelationClassesTable(kin)
#' relClasses$`Relationship Class` <-
#'   as.character(relClasses$`Relationship Class`)
#' relClassTbl <- kin[!kin$relation == "Self", ] %>%
#'   group_by(relation) %>%
#'   summarise(count = n())
#' relClassTbl
makeRelationClassesTable <- function(kin) {
  relationClass <- c(
    "Self", "Parent-Offspring", "Full-Siblings",
    "Half-Siblings", "Grandparent-Grandchild", "Full-Cousins",
    "Cousin - Other", "Full-Avuncular", "Avuncular - Other",
    "Other", "No Relation"
  )

  kin <- kin[kin$relation != "Self", ]
  r <- as.data.frame(table(kin$relation))
  colnames(r) <- c("Relationship Class", "Frequency")

  relationClass <- relationClass[relationClass %in% r[, "Relationship Class"]]
  r[match(relationClass, r[, "Relationship Class"]), ]
}
