#' Copyright(c) 2017-2023 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("cumulateSimKinships")

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
# nolint end: object_name_linter
set_seed(seed = 2L)
n <- 100L
simKinships <- cumulateSimKinships(ped, allSimParents, pop = ped$id, n = n)
testEN <- simKinships$meanKinship[
  seq_along(ped$id)[ped$id == "E"],
  seq_along(ped$id)[ped$id == "N"]
]

test_that("cumulateSimKinships creates the correct kinship summary structure", {
  expect_equal(testEN, 0.041250, tolerance = 0.000001)
  expect_length(simKinships, 4L)
  expect_equal(names(simKinships), c(
    "meanKinship", "sdKinship", "minKinship",
    "maxKinship"
  ))
  expect_equal(length(simKinships$meanKinship), 17L * 17L)
  expect_equal(nrow(simKinships$sdKinship), 17L)
})
