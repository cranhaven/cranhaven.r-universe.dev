test_that("clear_cache() runs without error", {
  expect_invisible(clear_cache())
})

test_that("obr_cache_dir() creates and returns a directory", {
  d <- obr_cache_dir()
  expect_type(d, "character")
  expect_true(dir.exists(d))
})
