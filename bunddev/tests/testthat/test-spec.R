test_that("bunddev_cache_dir returns a directory", {
  cache_dir <- bunddev_cache_dir()
  expect_true(dir.exists(cache_dir))
})

test_that("bunddev_spec_path builds a cache path", {
  spec_path <- bunddev_spec_path("abfallnavi")
  expect_true(grepl("abfallnavi", spec_path, fixed = TRUE))
  expect_true(grepl("\\.yaml$", spec_path))
})

test_that("bunddev_spec downloads and parses specs", {
  skip_if_offline()
  skip_on_cran()

  spec_path <- bunddev_spec_path("abfallnavi")
  if (file.exists(spec_path)) {
    unlink(spec_path)
  }

  spec <- bunddev_spec("abfallnavi", refresh = TRUE)
  expect_true(file.exists(spec_path))
  expect_true(is.list(spec))
})
