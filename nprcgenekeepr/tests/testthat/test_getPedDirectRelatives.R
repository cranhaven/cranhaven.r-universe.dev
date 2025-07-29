#' Copyright(c) 2017-2023 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getPedDirectRelatives")

test_that("getPedDirectRelatives throws an error with no pedigree", {
  expect_error(
    getPedDirectRelatives(),
    "Need to specify IDs"
  )
})

test_that("getPedDirectRelatives throws an error with no pedigree", {
  expect_null(getPedDirectRelatives(ids = "E", ped = NULL))
})

ped <- c("A", "B")
test_that("getPedDirectRelatives throws an error with no IDs", {
  expect_error(
    getPedDirectRelatives(ped = ped),
    "Need to specify IDs"
  )
})

test_that("getPedDirectRelatives throws an error with pedigree argument", {
  expect_error(
    getPedDirectRelatives(ids = "E"),
    "Need to specify pedigree"
  )
})

test_that(paste0(
  "getPedDirectRelatives throws an error with no data.frame ",
  "for pedigree"
), {
  expect_error(
    getPedDirectRelatives(ids = "E", ped = ped),
    "ped must be a data.frame object"
  )
})

ped <- nprcgenekeepr::lacy1989Ped
test_that("getPedDirectRelatives throws an error with no pedigree", {
  expect_error(
    getPedDirectRelatives(ped = ped),
    "Need to specify IDs"
  )
})

ped <- nprcgenekeepr::lacy1989Ped
ids <- "E"
relatives <- getPedDirectRelatives(
  ids = ids, ped = ped,
  unrelatedParents = FALSE
)
test_that("getPedDirectRelatives creates correct pedigree", {
  expect_setequal(relatives$id, c("A", "B", "C", "D", "E", "F", "G"))
})

ids <- "B"
relatives <- getPedDirectRelatives(
  ids = ids, ped = ped,
  unrelatedParents = FALSE
)
test_that("getPedDirectRelatives creates correct pedigree", {
  expect_setequal(relatives$id, c("A", "B", "C", "D", "E", "F", "G"))
})
ids <- "C"
relatives <- getPedDirectRelatives(
  ids = ids, ped = ped,
  unrelatedParents = FALSE
)
test_that("getPedDirectRelatives creates correct pedigree", {
  expect_setequal(relatives$id, c("A", "B", "C", "D", "E", "F", "G"))
})

ped2 <- rbind(ped, data.frame(
  id = c("H", "I", "J", "K", "L", "M"),
  sire = c("K", "K", "L", NA, NA, NA),
  dam = c(NA, "M", "M", NA, NA, NA),
  gen = rep(2L, 6L),
  population = rep(TRUE, 6L),
  stringsAsFactors = FALSE
))

ids <- "E"
relatives <- getPedDirectRelatives(
  ids = ids, ped = ped2,
  unrelatedParents = FALSE
)
test_that("getPedDirectRelatives creates correct pedigree", {
  expect_setequal(relatives$id, c("A", "B", "C", "D", "E", "F", "G"))
})

ids <- "B"
relatives <- getPedDirectRelatives(
  ids = ids, ped = ped2,
  unrelatedParents = FALSE
)
test_that("getPedDirectRelatives creates correct pedigree", {
  expect_setequal(relatives$id, c("A", "B", "C", "D", "E", "F", "G"))
})
ids <- "C"
relatives <- getPedDirectRelatives(
  ids = ids, ped = ped2,
  unrelatedParents = FALSE
)
test_that("getPedDirectRelatives creates correct pedigree", {
  expect_setequal(relatives$id, c("A", "B", "C", "D", "E", "F", "G"))
})
ids <- "M"
relatives <- getPedDirectRelatives(
  ids = ids, ped = ped2,
  unrelatedParents = FALSE
)
test_that("getPedDirectRelatives creates correct pedigree", {
  expect_setequal(relatives$id, c("H", "I", "J", "K", "L", "M"))
})
