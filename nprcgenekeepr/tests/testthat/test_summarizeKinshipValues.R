#' Copyright(c) 2017-2023 R. Mark Sharp
#' This file is part of nprcgenekeepr
library(testthat)
context("summarizeKinshipValues")
ped <- nprcgenekeepr::smallPed
# nolint start: object_name_linter.
simParent_1 <- list(
  id = "A",
  sires = c("s1_1", "s1_2", "s1_3"),
  dams = c("d1_1", "d1_2", "d1_3", "d1_4")
)
simParent_2 <- list(
  id = "B",
  sires = c("s1_1", "s1_2", "s1_3"),
  dams = c("d1_1", "d1_2", "d1_3", "d1_4")
)
simParent_3 <- list(
  id = "E",
  sires = c("A", "C", "s1_1"),
  dams = c("d3_1", "B")
)
simParent_4 <- list(
  id = "J",
  sires = c("A", "C", "s1_1"),
  dams = c("d3_1", "B")
)
simParent_5 <- list(
  id = "K",
  sires = c("A", "C", "s1_1"),
  dams = c("d3_1", "B")
)
simParent_6 <- list(
  id = "N",
  sires = c("A", "C", "s1_1"),
  dams = c("d3_1", "B")
)
allSimParents <- list(
  simParent_1, simParent_2, simParent_3,
  simParent_4, simParent_5, simParent_6
)

extractKinship <- function(simKinships, id1, id2, simulation) {
  ids <- dimnames(simKinships[[simulation]])[[1L]]
  simKinships[[simulation]][
    seq_along(ids)[ids == id1],
    seq_along(ids)[ids == id2]
  ]
}

extractKValue <- function(kValue, id1, id2, simulation) {
  kValue[kValue$id_1 == id1 & kValue$id_2 == id2, paste0("sim_", simulation)]
}

set_seed(seed = 1L)
n <- 10L
simKinships <- createSimKinships(ped, allSimParents, pop = ped$id, n = n)
kValues <- kinshipMatricesToKValues(simKinships)
counts <- countKinshipValues(kValues)
stats <- summarizeKinshipValues(counts)

test_that("summarizeKinshipValues makes correct structure", {
  expect_equal(length(stats), 9L)
  expect_equal(names(stats), c(
    "id_1", "id_2", "min", "secondQuartile",
    "mean", "median", "thirdQuartile", "max", "sd"
  ))

  expect_equal(length(stats$id_1), 153)
})
# nolint end: object_name_linter.

test_that("summarizeKinshipValues summarizes kinship values correctly", {
  expect_identical(stats$id_1[10L], "A")
  expect_identical(stats$id_2[10L], "J")
  expect_equal(stats$min[10L], 0L)
  expect_equal(stats$secondQuartile[10L], 0L)
  expect_equal(stats$mean[10L], 0, 01)
  expect_equal(stats$median[10L], 0L)
  expect_equal(stats$thirdQuartile[10L], 0.25)
  expect_equal(stats$max[10L], 0.25)
  expect_equal(stats$sd[10L], 0.1290994, tolerance = 0.00001)
})
