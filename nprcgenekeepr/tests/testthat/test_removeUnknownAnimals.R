#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("removeUnknownAnimals")
library(testthat)
library(stringi)
ped <- nprcgenekeepr::smallPed
newPed <- cbind(ped,
  recordStatus = rep("original", nrow(ped)),
  stringsAsFactors = FALSE
)
addedPed <- newPed
addedPed[1L:3L, "recordStatus"] <- "added"
test_that(stri_c(
  "removeUnknownAnimals removes nothing with no \"added\" ",
  "(unknown) animals"
), {
  ped1 <- removeUnknownAnimals(newPed)
  expect_identical(nrow(ped1), nrow(newPed))
  expect_identical(nrow(ped1), nrow(addedPed))
})

test_that(stri_c(
  "removeUnknownAnimals removes \"added\" ",
  "(unknown) animals"
), {
  ped2 <- removeUnknownAnimals(addedPed)
  expect_false(nrow(ped2) == nrow(addedPed))
  expect_identical(nrow(ped2) + 3L, nrow(addedPed))
})
