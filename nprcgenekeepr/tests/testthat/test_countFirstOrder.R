#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("countFirstOrder")
library(testthat)
data("lacy1989Ped")
ped <- lacy1989Ped
ids <- c("A", "B", "D", "E", "F", "G")
countIds <- countFirstOrder(ped, ids)
count <- countFirstOrder(ped, NULL)

test_that("countFirstOrder makes correct transformations", {
  expect_identical(countIds$id[countIds$id %in% count$id], countIds$id)
  expect_true(all(count$id[count$id %in% countIds$id] %in% countIds$id))
  expect_identical(count$parents, c(0L, 0L, 2L, 2L, 0L, 2L, 2L))
  expect_identical(count$offspring, c(2L, 2L, 0L, 2L, 2L, 0L, 0L))
  expect_identical(count$siblings, c(0L, 0L, 1L, 1L, 0L, 1L, 1L))
  expect_identical(count$total, c(2L, 2L, 3L, 5L, 2L, 3L, 3L))
})
