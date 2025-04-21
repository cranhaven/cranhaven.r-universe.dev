library(mnis)
context("mnis_eligible")


test_that("mnis_eligible returns expected format", {
  skip_on_cran()

  xmniselig <- mnis_eligible()
  expect_type(xmniselig, "list")
  expect_true(tibble::is_tibble(xmniselig))
})
