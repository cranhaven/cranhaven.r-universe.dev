library(stringr)
library(stats)
library(handyFunctions)
library(ggplot2)

data(SNV_1MB_density_data)
test_that("multiplication works", {
  expect_output(ShowSNPDensityPlot(SNV_1MB_density_data, binSize = 1e6, chromSet = c(38:1)),regexp = NA)
})
