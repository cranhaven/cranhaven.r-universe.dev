test_that("scrap_stats works", {
  testthat::skip_on_cran()

  res <- scrap_stats(11190594)

  expect_equal(nrow(res), 2)
  expect_equal(length(res), 6)
})
