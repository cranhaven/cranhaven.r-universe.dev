#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("createPedOne")
pedOne <- createPedOne(savePed = FALSE)
test_that("createPedOne makes the right pedigree", {
  expect_identical(nrow(pedOne), 8L)
  expect_identical(ncol(pedOne), 5L)
  expect_identical(names(pedOne)[1L], "ego_id")
})
pedOne <- createPedOne(savePed = TRUE)
test_that("createPedOne makes the right pedigree and saves it", {
  expect_identical(names(pedOne)[1L], "ego_id")
})
