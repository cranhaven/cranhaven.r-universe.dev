#' Copyright(c) 2017-2024 R. Mark Sharp
#' This file is part of nprcgenekeepr
context("getPyramidPlot")
library(testthat)
recPlot <- function(expr) {
  pdf(NULL)
  on.exit(dev.off())
  dev.control(displaylist = "enable")
  expr
  recordPlot()
}
agePlot <- recPlot(getPyramidPlot(nprcgenekeepr::qcPed))
test_that("getPyramidPlot generates a plot with or without pedigree", {
  expect_s3_class(agePlot, "recordedplot")
  expect_s3_class(recPlot(getPyramidPlot(NULL)), "recordedplot")
})
