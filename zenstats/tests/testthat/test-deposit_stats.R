test_that("deposit_stats works", {
  testthat::skip_on_cran()

  ids <- c(11190594, 11193971, 11126924)
  res <- deposit_stats(deposit_ids = ids, progress = FALSE)

  expect_equal(nrow(res), 6)
  expect_equal(length(res), 6)
})
