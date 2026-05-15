test_that("item_data returns list with original_data + metrics", {
  skip_on_cran() # mgcv + GAMs can be slower on CRAN
  fit <- .fit_mixed(n = 500, n_cat = 4, thr = c(-1, 0, 1))  # mixed gives both branches to exercise code paths
  res <- item_data(fit, store_fits = FALSE, progress = FALSE, verbose = FALSE)
  expect_true(is.list(res))
  expect_true(all(c("original_data","metrics") %in% names(res)))
  expect_s3_class(res$original_data, "data.frame")
  expect_s3_class(res$metrics, "data.frame")
  expect_true(all(c("r2","rmse","mae") %in% names(res$metrics)))
})
