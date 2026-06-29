
# rm(list = ls()); library(testthat); library(arctools)

context("Testing get_actigraph_SN()")

test_that("Test if ActigraphSN returns correct Actigraph serial number.", {
  skip_on_cran()

  expect_equal(
    get_actigraph_SN(system.file("extdata", extdata_fnames[1], package = "arctools")),
    "TAS1D47140221"
    )

})
