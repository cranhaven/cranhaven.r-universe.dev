test_that("pdo_download", {
  rlang::local_options(lifecycle_verbosity = "quiet")

  expect_is(download_pdo(), "data.frame")
  expect_is(pdo_download(), "data.frame")

  lifecycle::expect_deprecated(download_pdo())
  lifecycle::expect_deprecated(pdo_download())
})
