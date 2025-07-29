#' Copyright(c) 2017-2023 R. Mark Sharp
#' This file is part of nprcgenekeepr
library(testthat)
context("kinshipMatricesToKValues")

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
  # nolint start: object_usage_linter.
  kValue[id_1 == id1 & id_2 == id2, paste0("sim_", simulation),
    with = FALSE
  ][[1L]]
  # nolint end: object_usage_linter.
}
# nolint end: object_usage_linter.
set_seed(seed = 1L)
n <- 10L
pop <- ped$id
nPop <- length(pop)
expectedNRows <- nPop + nPop * (nPop - 1L) / 2L
expectedNCols <- 2L + n
simKinships <- createSimKinships(ped, allSimParents, pop = pop, n = n)
kValue <- kinshipMatricesToKValues(simKinships)
test_that("kinshipMatricesToKValues gets correct number of rows", {
  expect_equal(nrow(kValue), expectedNRows)
})
test_that("kinshipMatricesToKValues gets correct number of columns", {
  expect_length(kValue, expectedNCols)
})

test_that("kinshipMatricesToKValues gets correct kinship values", {
  expect_identical(
    extractKinship(simKinships, "A", "B", 2L),
    extractKValue(kValue, id1 = "A", id2 = "B", simulation = 2L)
  )
  expect_identical(
    extractKinship(simKinships, "A", "C", 2L),
    extractKValue(kValue, id1 = "A", id2 = "C", simulation = 2L)
  )
  expect_identical(
    extractKinship(simKinships, "A", "D", 2L),
    extractKValue(kValue, id1 = "A", id2 = "D", simulation = 2L)
  )
  expect_identical(
    extractKinship(simKinships, "A", "E", 2L),
    extractKValue(kValue, id1 = "A", id2 = "E", simulation = 2L)
  )
  expect_identical(
    extractKinship(simKinships, "A", "B", 3L),
    extractKValue(kValue, id1 = "A", id2 = "B", simulation = 3L)
  )
})
