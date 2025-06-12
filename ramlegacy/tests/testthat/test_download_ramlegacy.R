context("Testing that download_ramlegacy works")

test_that("defaults to curr. latest version 4.44 if version not specified", {
  skip_on_cran()
  temp_path <- tempfile("ramlegacy", tempdir())
  download_ramlegacy(NULL, temp_path)
  vers_path <- file.path(temp_path, "4.44")
  rds_path <- file.path(vers_path, "RLSADB v4.44")
  rds_path <- file.path(rds_path, "DB Files With Assessment Data")
  rds_path <- file.path(rds_path, "v4.44.rds")
  expect_true(file.exists(rds_path))
  unlink(rds_path, recursive = TRUE)
})


test_that("Overwrites existing download when overwrite = TRUE", {
  skip_on_cran()
  temp_path <- tempfile("ramlegacy", tempdir())
  # download version 4.44 for the first time
  download_ramlegacy("4.44", temp_path)
  # call download_ramlegacy again to test behavior
  expect_true(download_ramlegacy("4.44", temp_path, overwrite = TRUE))
  unlink(temp_path, recursive = TRUE)
})

# testing download_ramlegacy downloads older version 1.0
test_that("download_ramlegacy downloads v1.0", {
  skip_on_cran()
  temp_path <- tempfile("ramlegacy", tempdir())
  download_ramlegacy("1.0", temp_path)
  vers_path <- file.path(temp_path, "1.0")
  rds_path <- file.path(vers_path, "v1.0.rds")
  expect_true(file.exists(rds_path))
  unlink(rds_path, recursive = TRUE)
})
