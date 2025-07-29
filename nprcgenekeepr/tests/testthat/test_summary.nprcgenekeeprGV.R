#' Copyright(c) 2017-2024 R. Mark Sharp
# This file is part of nprcgenekeepr
context("summary.nprcgenekeeprGV")
library(testthat)

test_that("summary.nprcgenekeeprGV provides expected output", {
  skip_on_cran()
  set_seed(10L)
  ped <- nprcgenekeepr::pedOne
  ped$birth_date[ped$ego_id == "d2"] <- "2000-04-13"
  ped$birth_date[ped$ego_id == "o4"] <- "2016-04-13"
  ped <- suppressWarnings(qcStudbook(ped, reportErrors = FALSE))
  gvReport <- reportGV(ped, guIter = 10L)
  summaryGV <- summary(gvReport)
  expect_identical(summaryGV[1L], "The genetic value report")
  expect_length(summaryGV, 8L)
})
