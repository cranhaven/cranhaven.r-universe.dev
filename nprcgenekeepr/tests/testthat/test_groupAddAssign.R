#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("groupAddAssign")
library(testthat)
library(nprcgenekeepr)
qcBreeders <- nprcgenekeepr::qcBreeders
pedWithGenotype <- nprcgenekeepr::pedWithGenotype
pedWithGenotypeReport <- nprcgenekeepr::pedWithGenotypeReport
skip_if_not(exists("qcBreeders"))
skip_if_not(exists("pedWithGenotype"))
skip_if_not(exists("pedWithGenotypeReport"))
set_seed(10L)
test_that("groupAddAssign forms the correct groups", {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  currentGroups <- list(1L)
  currentGroups[[1L]] <- qcBreeders[1L:3L]
  groupAddTest <- groupAddAssign(
    candidates = qcBreeders,
    kmat = pedWithGenotypeReport$kinship,
    ped = pedWithGenotype,
    currentGroups = currentGroups,
    ignore = NULL, minAge = 1.0, numGp = 1L,
    harem = FALSE, sexRatio = 0L, withKin = FALSE
  )
  expect_length(groupAddTest$group[[1L]], 11L)
  expect_length(groupAddTest$group[[2L]], 14L)
  # expect_equal(length(groupAddTest$group[[2L]]), 10L)
  expect_null(groupAddTest$groupKin[[1L]])
})
set_seed(10L)
test_that("groupAddAssign (numGp = 2) forms the correct groups", {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  groupAssignTest <- groupAddAssign(
    candidates = qcBreeders,
    kmat = pedWithGenotypeReport$kinship,
    ped = pedWithGenotype,
    currentGroups = character(0L),
    ignore = NULL,
    minAge = 1L,
    numGp = 2L,
    harem = FALSE,
    sexRatio = 0.0,
    withKin = FALSE
  )
  expect_length(groupAssignTest$group[[1L]], 9L)
  # expect_equal(length(groupAssignTest$group[[2L]]), 10L)
  expect_length(groupAssignTest$group[[2L]], 9L)
  expect_null(groupAssignTest$groupKin[[1L]])
})
set_seed(10L)
test_that(paste0(
  "groupAddAssign (numGp = 1) forms the correct groups with ",
  "kinship matrices"
), {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  currentGroups <- list(1L)
  currentGroups[[1L]] <- qcBreeders[1L:3L]
  groupAddKTest <- groupAddAssign(
    candidates = qcBreeders,
    kmat = pedWithGenotypeReport$kinship,
    ped = pedWithGenotype,
    currentGroups = currentGroups,
    ignore = NULL,
    minAge = 1L,
    numGp = 1L,
    harem = FALSE,
    sexRatio = 0.0,
    withKin = TRUE
  )
  expect_length(groupAddKTest$group[[1L]], 11L)
  expect_length(groupAddKTest$group[[2L]], 14L)
  # expect_equal(length(groupAddKTest$group[[2L]]), 10L)
})
set_seed(10L)
test_that("groupAddAssign forms the correct groups with kinship matrices", {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  groupAssignKTest <- groupAddAssign(
    candidates = qcBreeders,
    kmat = pedWithGenotypeReport$kinship,
    ped = pedWithGenotype,
    currentGroups = character(0L),
    ignore = NULL,
    minAge = 1.0,
    numGp = 2L,
    harem = FALSE,
    sexRatio = 0.0,
    withKin = TRUE
  )
  expect_equal(length(groupAssignKTest$group[[1L]]), 9L)
  expect_length(groupAssignKTest$group[[2L]], 9L)
  # expect_equal(length(groupAssignKTest$group[[2L]]), 10L)
  expect_length(groupAssignKTest$groupKin[[1L]], 81L)
})
set_seed(10L)
noSires <- removePotentialSires(qcBreeders,
  minAge = 2.0,
  pedWithGenotype
)
sires <- getPotentialSires(qcBreeders, pedWithGenotype, minAge = 2.0)

test_that(paste0(
  "groupAddAssign fails when no potential sires exist for harem creation"
), {
  skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
  expect_error(
    groupAddAssign(
      candidates = noSires,
      kmat = pedWithGenotypeReport$kinship,
      ped = pedWithGenotype,
      currentGroups = character(0L),
      ignore = NULL,
      minAge = 1.0,
      numGp = 2L,
      harem = TRUE,
      sexRatio = 0.0,
      withKin = TRUE
    )
  )
})
test_that(
  paste0(
    "groupAddAssign add 1 sire at most when there are multiple potential ",
    "sires in the candidates during harem creation"
  ),
  {
    skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
    group <- groupAddAssign(
      candidates = qcBreeders,
      kmat = pedWithGenotypeReport$kinship,
      ped = pedWithGenotype,
      currentGroups = character(0L),
      ignore = NULL,
      minAge = 1.0,
      numGp = 2L,
      harem = TRUE,
      sexRatio = 0.0,
      withKin = TRUE
    )
    expect_true(length(group) == 3L)
    expect_identical(sum(seq_along(group[[1L]][[3L]])[group[[1L]][[3L]] %in%
                                                  sires]), 0L)
    expect_identical(sum(seq_along(group[[1L]][[3L]])[group[[1L]][[2L]] %in%
                                                    sires]), 1L)
  }
)
test_that(
  paste0(
    "groupAddAssign fails when there are more groups with seed animals that ",
    "the number of groups to be formed"
  ),
  {
    skip_if_not(Sys.info()[names(Sys.info()) == "user"] == "rmsharp")
    currentGroups <- list(3L)
    currentGroups[[1L]] <- qcBreeders[1L:3L]
    currentGroups[[2L]] <- qcBreeders[4L:6L]
    currentGroups[[3L]] <- qcBreeders[7L:9L]
    expect_error(
      groupAddAssign(
        candidates = noSires,
        kmat = pedWithGenotypeReport$kinship,
        ped = pedWithGenotype,
        currentGroups = currentGroups,
        ignore = NULL,
        minAge = 1.0,
        numGp = 2L,
        harem = FALSE,
        sexRatio = 0L,
        withKin = TRUE
      )
    )
  }
)
