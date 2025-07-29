#' Copyright(c) 2017-2023 R. Mark Sharp
#' This file is part of nprcgenekeepr

context("countKinshipValues")
# nolint start: object_name_linter
ped <- nprcgenekeepr::smallPed
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
extractKValue <- function(kValue, id1, id2, simulation) {
  kValue[id_1 == id1 & id_2 == id2, paste0("sim_", simulation),
         with = FALSE][[1L]]
}
# nolint end: object_name_linter
set_seed(seed = 1L)
n <- 10L
simKinships <- createSimKinships(ped, allSimParents, pop = ped$id, n = n)
kValues <- kinshipMatricesToKValues(simKinships)
counts <- countKinshipValues(kValues)
simKinships <- createSimKinships(ped, allSimParents, pop = ped$id, n = n)
kValues <- kinshipMatricesToKValues(simKinships)
cummulatedCounts <- countKinshipValues(kValues, counts)

test_that("countKinshipValues detects contaminated ID list", {
  counts$kIds[[1L]] <- c("badID_1", "badID_2")
  suppressWarnings(expect_error(
    countKinshipValues(kValues, counts),
    "ID pairs in simulated pedigrees do not match:"
  ))
})
test_that("countKinshipValues makes correct structure", {
  expect_length(counts, 3L)
  expect_equal(names(counts), c("kIds", "kValues", "kCounts"), with = FALSE)
  expect_length(counts$kIds, 153L)
})

test_that("countKinshipValues counts kinship values correctly", {
  expect_equal(counts$kCounts[[10L]], c(6L, 4L))
  expect_equal(counts$kValues[[7L]], c(0.125, 0.25))
  expect_identical(as.character(counts$kIds[[3L]]), c("A", "C"))
})

test_that("countKinshipValues makes correct structure", {
  expect_length(cummulatedCounts, 3L)
  expect_identical(names(cummulatedCounts), c("kIds", "kValues", "kCounts"))
  expect_length(cummulatedCounts$kIds, 153L)
})

test_that("countKinshipValues counts kinship values correctly", {
  expect_equal(cummulatedCounts$kCounts[[10]], c(14L, 6L))
  expect_equal(cummulatedCounts$kValues[[7]], c(0.125, 0.25))
  expect_identical(as.character(cummulatedCounts$kIds[[3L]]), c("A", "C"))
})
