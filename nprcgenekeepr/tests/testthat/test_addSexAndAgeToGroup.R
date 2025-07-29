#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("addSexAndAgeToGroup")

data("qcBreeders")
data("qcPed")
skip_if_not(exists("qcBreeders"))
skip_if_not(exists("qcPed"))
test_that("addSexAndAgeToGroup forms the correct dataframe", {
  df <- addSexAndAgeToGroup(ids = qcBreeders, ped = qcPed)
  expect_length(df, 3L)
  expect_length(df[["ids"]], 29L)
  expect_named(df, c("ids", "sex", "age"))
  expect_identical(df$ids[1L], "Q0RGP7")
  expect_identical(as.character(df$sex[1L]), "F")
})
