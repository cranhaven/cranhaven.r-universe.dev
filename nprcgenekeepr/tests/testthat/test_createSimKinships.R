#' Copyright(c) 2017-2023 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("createSimKinships")

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

extractKinship <- function(simKinships, id1, id2) {
  vapply(simKinships,
    function(x) {
      x[
        seq_along(ped$id)[ped$id == id1],
        seq_along(ped$id)[ped$id == id2]
      ]
    },
    FUN.VALUE = numeric(1L)
  )
}

set_seed(seed = 1L)
n <- 10L
simKinships <- createSimKinships(ped, allSimParents, pop = ped$id, n = n)
test_EN <- extractKinship(simKinships, "E", "N")
test_BN <- extractKinship(simKinships, "B", "N")
test_JN <- extractKinship(simKinships, "J", "N")
test_KN <- extractKinship(simKinships, "K", "N")
test_BK <- extractKinship(simKinships, "B", "K")
test_EK <- extractKinship(simKinships, "E", "K")
# nolint end: object_name_linter

test_that("createSimKinships creates the correct kinship matrices structure", {
  expect_equal(length(simKinships), n)
  expect_equal(length(simKinships[[1L]]), 17L * 17L)
  expect_equal(nrow(simKinships[[1L]]), 17L)
  expect_equal(test_EN, c(0.125, 0.0, 0.0, 0.0, 0.125, 0.125, 0.0, 0.0, 0.0,
                          0.125))
})
