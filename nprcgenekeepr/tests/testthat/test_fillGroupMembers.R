#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("fillGroupMembers")

test_that("fillGroupMembers adds animals in the specified sex ratio", {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  examplePedigree <- nprcgenekeepr::examplePedigree
  set_seed(10L)
  ped <- qcStudbook(examplePedigree,
    minParentAge = 2.0, reportChanges = FALSE,
    reportErrors = FALSE
  )

  kmat <- kinship(ped$id, ped$sire, ped$dam, ped$gen, sparse = FALSE)
  currentGroups <- list(1L)
  currentGroups[[1L]] <- examplePedigree$id[1L:3L]
  candidates <- examplePedigree$id[examplePedigree$status == "ALIVE"]
  threshold <- 0.015625
  kin <- getAnimalsWithHighKinship(kmat, ped, threshold, currentGroups,
    ignore = list(c("F", "F")), minAge = 1
  )
  # Filtering out candidates related to current group members
  conflicts <- unique(c(
    unlist(kin[unlist(currentGroups)]),
    unlist(currentGroups)
  ))
  candidates <- setdiff(candidates, conflicts)


  kin <- addAnimalsWithNoRelative(kin, candidates)

  ignore <- NULL
  minAge <- 1.0
  harem <- FALSE
  numGp <- 1L
  withKin <- FALSE

  sexRatio <- 0.0
  groupMembers <- nprcgenekeepr:::fillGroupMembers(
    candidates, currentGroups, kin, ped, harem,
    minAge, numGp, sexRatio
  )
  expect_equal(groupMembers[[1L]][1L:3L], c("N54ICI", "VJ08BW", "2ZMHG7"))
  expect_equal(calculateSexRatio(groupMembers[[1L]], ped), 52.5,
    tolerance = 0.1, scale = 1.0
  )
  sexRatio <- 1L
  groupMembers <- nprcgenekeepr:::fillGroupMembers(
    candidates, currentGroups, kin, ped, harem,
    minAge, numGp, sexRatio
  )
  expect_equal(
    groupMembers[[1L]][1L:4L],
    c("N54ICI", "VJ08BW", "2ZMHG7", "CS23RV")
  )
  expect_equal(calculateSexRatio(groupMembers[[1]], ped), 1.0,
    tolerance = 0.1, scale = 1.0
  )
})
