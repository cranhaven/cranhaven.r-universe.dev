library(ncdump)
library(testthat)
context("Basic read")

ifile <- system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package = "ncdump")
con <- NetCDF(ifile)
test_that("File exists and can be read", {
  expect_true(file.exists(ifile))
  expect_that(con, is_a("ncdump"))
  expect_that(vars(con), is_a("tbl_df"))
  expect_that(atts(con), is_a("tbl_df"))
  expect_that(dims(con), is_a("tbl_df"))
})


context("Differentiate attributes global and var-based")
test_that("get global attributes", {
  expect_that(atts(con), is_a("tbl_df"))
  ## something changed here, need to explore 
  #expect_that(atts(con, "chlor_a"), is_a("tbl_df"))
  expect_that(dimvars(con), is_a("tbl_df"))
})
