#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("print.summary.nprcgenekeeprGV")
library(testthat)

test_that("print.summary.nprcgenekeeprGV prints expected output", {
  ped <- nprcgenekeepr::pedOne
  ped$birth_date[ped$ego_id == "d2"] <- "2000-04-13"
  ped$birth_date[ped$ego_id == "o4"] <- "2016-04-13"
  ped <- suppressWarnings(qcStudbook(ped, reportErrors = FALSE))
  summaryGV <- summary(reportGV(ped, guIter = 10L))
  expect_success(expect_output(print(summaryGV)))
  expect_identical(summaryGV[1L], "The genetic value report")
  expect_length(summaryGV, 8L)
})
