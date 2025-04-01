context("Print and Summary")
library(clusterability)

test_that("print - errors", {
  iris1 <- data(iris)
  iris1 <- iris[,1:4]

  cresult <- clusterabilitytest(iris1, test = "Dip", reduction = "PCA")

  expect_error(print.clusterability(iris1), "clusterability", info = "Expect error for incorrect class types")
})

test_that("print - conditionals", {
  iris1 <- data(iris)
  iris1 <- iris[,1:4]

  miss <- matrix(c(1, 5, 8, NA, 2, 3, 8, 7, 9, 4, 2, 4, 6, 9, 11, 4, 13, 5, 6, 1, 0), ncol = 3)

  cresult <- clusterabilitytest(iris1, test = "Dip", reduction = "PCA", d_simulatepvalue = TRUE, d_reps = 500)
  missresult <- clusterabilitytest(miss, test = "Silverman", reduction = "distance",distance_metric = "euclidean", distance_standardize = "STD", s_m = 1422, s_digits = 7, s_setseed = 1234, s_outseed = TRUE, completecase = TRUE)

  basicdip <- clusterabilitytest(iris1, test = "dip")
  basicsilv <- clusterabilitytest(iris1, test = "silverman")

  # Missing or No Missing
  expect_output(print.clusterability(missresult), "missing at least one")
  expect_output(print.clusterability(cresult), "is complete")

  # Data reduction technique
  expect_output(print.clusterability(cresult), "PCA")
  expect_output(print.clusterability(missresult), "Distance")
  expect_output(print.clusterability(missresult), "euclidean")
  expect_output(print.clusterability(missresult), "STD")

  # Descriptions of data reduction
  expect_output(print.clusterability(missresult), "has mean 0")


  # Test name
  expect_output(print.clusterability(cresult), "Dip Test")
  expect_output(print.clusterability(missresult),  "Silverman")

  # Null and Alternative
  expect_output(print.clusterability(cresult), "Null Hypothesis")
  expect_output(print.clusterability(cresult), "Alternative Hypothesis")
  expect_output(print.clusterability(cresult), "p-value")
  expect_output(print.clusterability(cresult), "Dip statistic")

  expect_output(print.clusterability(missresult), "Null Hypothesis")
  expect_output(print.clusterability(missresult), "Alternative Hypothesis")
  expect_output(print.clusterability(missresult), "p-value")
  expect_output(print.clusterability(missresult), "Critical bandwidth")

  # Test options
  expect_output(print.clusterability(cresult), "Test Options")
  expect_output(print.clusterability(missresult), "Test Options")

  # Dip
  expect_output(print.clusterability(cresult), "Monte Carlo")
  expect_output(print.clusterability(cresult), "500")

  expect_output(print.clusterability(basicdip), "Default")

  # Silverman
  expect_output(print.clusterability(missresult), "Seed set")
  expect_output(print.clusterability(missresult), "1234")

  expect_output(print.clusterability(missresult), "1422 bootstrap")

  expect_output(print.clusterability(missresult), "Hall and York")

  expect_output(print.clusterability(missresult), "7 digits")
})
