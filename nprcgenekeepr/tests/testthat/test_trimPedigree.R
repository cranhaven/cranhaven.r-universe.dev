#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("trimPedigree")
library(testthat)

data("smallPed")
ped <- smallPed
trim1 <- trimPedigree(c("C", "K", "L", "D", "E"), ped,
  removeUninformative = FALSE,
  addBackParents = FALSE
)
trim2 <- trimPedigree(c("C", "K", "L", "D", "E"), ped,
  removeUninformative = TRUE,
  addBackParents = FALSE
)
trim3 <- trimPedigree(c("C", "K", "L", "D", "E"), ped,
  removeUninformative = FALSE,
  addBackParents = TRUE
)
trim4 <- trimPedigree(c("C", "K", "L", "D", "E"), ped,
  removeUninformative = TRUE,
  addBackParents = TRUE
)
eTrim1 <- c("A", "B", "C", "D", "E", "K", "L", "Q")
eTrim2 <- c("A", "B", "C", "D", "E", "L")
eTrim3 <- c("A", "B", "C", "D", "E", "K", "L", "Q")
eTrim4 <- c("A", "B", "C", "D", "E", "L", "K")
test_that("trimPedigree retains the correct egos", {
  expect_true(all(trim1$id %in% eTrim1))
  expect_true(all(eTrim1 %in% trim1$id))
  expect_true(all(trim2$id %in% eTrim2))
  expect_true(all(eTrim2 %in% trim2$id))
  expect_true(all(trim3$id %in% eTrim3))
  expect_true(all(eTrim3 %in% trim3$id))
  expect_true(all(trim4$id %in% eTrim4))
  expect_true(all(eTrim4 %in% trim4$id))
})
