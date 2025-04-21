library(mnis)
context("full_biog")


test_that("mnis_full_biog returns expected format", {
  skip_on_cran()


  xfb <- mnis_full_biog(172)

  expect_type(xfb, "list")
})
