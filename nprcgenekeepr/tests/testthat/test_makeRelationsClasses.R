#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("makeRelationsClasses")
library(testthat)
suppressMessages(library(dplyr))

qcPed <- nprcgenekeepr::qcPed
bkmat <- kinship(qcPed$id, qcPed$sire, qcPed$dam, qcPed$gen,
  sparse = FALSE
)
kin <- convertRelationships(bkmat, qcPed)
relClasses <- as.data.frame(makeRelationClassesTable(kin))
relClasses$`Relationship Class` <- as.character(relClasses$`Relationship Class`)
relClassTbl <- kin[!kin$relation == "Self", ] %>%
  group_by(relation) %>%
  summarise(count = n())
test_that("makeRelationsClasses retains the correct counts", {
  for (rel in relClasses[, "Relationship Class"]) {
    expect_identical(
      relClasses$Frequency[relClasses$`Relationship Class` == rel],
      relClassTbl$count[relClassTbl$relation == rel]
    )
  }
})
