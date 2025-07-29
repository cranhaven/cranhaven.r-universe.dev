#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("makeGroupMembers")
library(testthat)
library(nprcgenekeepr)
qcBreeders <- nprcgenekeepr::qcBreeders
pedWithGenotype <- nprcgenekeepr::pedWithGenotype
pedWithGenotypeReport <- nprcgenekeepr::pedWithGenotypeReport
skip_if_not(exists("qcBreeders"))
skip_if_not(exists("pedWithGenotype"))
skip_if_not(exists("pedWithGenotypeReport"))
set_seed(10L)
test_that(paste0(
  "makeGroupMembers fails when more than one potential sire ",
  "exists in currentGroup and harem == TRUE."
), {
  currentGroups <- list(1L)
  currentGroups[[1L]] <- qcBreeders
  expect_error(makeGroupMembers(
    numGp = 3L, currentGroups = currentGroups,
    candidates = qcBreeders, ped = pedWithGenotype, harem = TRUE,
    minAge = 2.0
  ), paste0(
    "User selected to form harems with more than one ",
    "male, There are 3 at least 2 years old in the ",
    "current group."
  ))
})

test_that(paste0(
  "makeGroupMembers fails when the number of potential sires ",
  "in candidates is less than the number of groups being ",
  "formed, there is not a current group, and harem == TRUE."
), {
  noSires <- removePotentialSires(
    ids = qcBreeders, minAge = 2.0,
    ped = pedWithGenotype
  )
  currentGroups <- list(1L)
  expect_error(makeGroupMembers(
    numGp = 3L, currentGroups = character(0L),
    candidates = noSires,
    ped = pedWithGenotype, harem = TRUE,
    minAge = 2.0
  ), paste0(
    "User selected to form harems in 3 groups with only ",
    "0 males at least 2 years old in the list of ",
    "candidates."
  ))
})

test_that(paste0(
  "makeGroupMembers initializes groupMembers correctly when the number of ",
  "potential sires in currentGroup is one and the candidate animals contain ",
  "one or more potential sires and harem == TRUE."
), {
  currentGroups <- list(1L)
  noSires <- removePotentialSires(
    ids = qcBreeders, minAge = 2L,
    ped = pedWithGenotype
  )
  sires <- getPotentialSires(qcBreeders, ped = pedWithGenotype, minAge = 2.0)
  currentGroups[[1L]] <- c(noSires[1L:10L], sires[1L])
  candidates <- c(noSires[11L:length(noSires)], sires[-1L])
  groupMembers <- makeGroupMembers(
    numGp = 1L, currentGroups = currentGroups, candidates = candidates,
    ped = pedWithGenotype, harem = TRUE, minAge = 2.0
  )
  expect_length(groupMembers, 1L)
  expect_identical(groupMembers[[1L]], currentGroups[[1L]])
})
test_that(paste0(
  "makeGroupMembers initializes groupMembers correctly when ",
  "harem == TRUE, there are no animals in the currentGroup and the candidate ",
  "animals contain numGp or more potential sires"
), {
  currentGroups <- character(0L)
  candidates <- qcBreeders
  groupMembers <- makeGroupMembers(
    numGp = 3L, currentGroups = currentGroups,
    candidates = candidates,
    ped = pedWithGenotype, harem = TRUE,
    minAge = 2.0
  )
  expect_length(groupMembers, 3L)
  expect_identical(class(groupMembers[[1L]][1L]), "character")
  expect_length(groupMembers[[1L]][1L], 1L)
})
test_that(paste0(
  "makeGroupMembers initializes groupMembers correctly when the number of ",
  "potential sires in currentGroup is zero and the candidate animals contain ",
  "one or more potential sires"
), {
  currentGroups <- list(1L)
  noSires <- removePotentialSires(
    ids = qcBreeders, minAge = 2.0,
    ped = pedWithGenotype
  )
  sires <- getPotentialSires(qcBreeders, ped = pedWithGenotype, minAge = 2.0)
  currentGroups[[1L]] <- c(noSires[1L:10L])
  candidates <- c(noSires[11L:length(noSires)], sires)
  groupMembers <- makeGroupMembers(
    numGp = 1L, currentGroups = currentGroups,
    candidates = candidates,
    ped = pedWithGenotype, harem = TRUE,
    minAge = 2.0
  )
  expect_length(groupMembers, 1L)
  expect_length(groupMembers[[1L]], 11L)
  expect_identical(
    groupMembers[[1L]][-1L],
    c(
      "Q0RGP7", "C1ICXL", "2KULR3", "RI0O7F", "7M51X5", "170ZTZ",
      "CFPEEU", "CQC133", "ZC5SCR", "218FOV"
    )
  )
})
test_that(paste0(
  "makeGroupMembers initializes groupMembers correctly when the number of ",
  "potential sires in currentGroup is 1 and the candidate animals contain ",
  "no potential sires"
), {
  currentGroups <- list(1L)
  noSires <- removePotentialSires(
    ids = qcBreeders, minAge = 2L,
    ped = pedWithGenotype
  )
  sires <- getPotentialSires(qcBreeders, ped = pedWithGenotype, minAge = 2L)
  currentGroups[[1L]] <- c(noSires[1L:10L], sires[1L])
  candidates <- noSires[11L:length(noSires)]
  groupMembers <- makeGroupMembers(
    numGp = 1L, currentGroups = currentGroups,
    candidates = candidates,
    ped = pedWithGenotype, harem = TRUE,
    minAge = 2L
  )
  expect_length(groupMembers, 1L)
  expect_length(groupMembers[[1L]], 11L)
  expect_identical(groupMembers[[1L]], c(noSires[1L:10L], sires[1L]))
})
