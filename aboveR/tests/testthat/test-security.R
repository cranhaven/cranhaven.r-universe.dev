# Tests for security functions

test_that("sanitize_filename() strips path traversal", {
  expect_equal(
    sanitize_filename("https://example.com/../../etc/passwd"),
    "passwd"
  )
  expect_equal(
    sanitize_filename("https://example.com/path/to/file.tif"),
    "file.tif"
  )
})

test_that("sanitize_filename() removes dangerous characters", {
  result <- sanitize_filename("https://example.com/tile:name<>.tif")
  expect_false(grepl("[:<>]", result))
})

test_that("sanitize_filename() removes leading dots", {
  result <- sanitize_filename("https://example.com/...hidden")
  expect_false(startsWith(result, "."))
})

test_that("sanitize_filename() handles empty result", {
  result <- sanitize_filename("https://example.com/...")
  expect_true(nzchar(result))
})

test_that("validate_kfa_url() accepts valid KFA URLs", {
  expect_invisible(
    validate_kfa_url("https://kyfromabove.s3.us-west-2.amazonaws.com/elevation/DEM/Phase2/tile.tif")
  )
})

test_that("validate_kfa_url() rejects non-KFA URLs", {
  expect_error(
    validate_kfa_url("https://evil.com/malware.exe"),
    "does not point to the KyFromAbove S3 bucket"
  )
  expect_error(
    validate_kfa_url("http://kyfromabove.s3.us-west-2.amazonaws.com/file.tif"),
    "does not point"
  )
})

test_that("validate_kfa_urls() validates all URLs in vector", {
  urls <- c(
    "https://kyfromabove.s3.us-west-2.amazonaws.com/a.tif",
    "https://kyfromabove.s3.us-west-2.amazonaws.com/b.tif"
  )
  expect_invisible(validate_kfa_urls(urls))

  bad_urls <- c(
    "https://kyfromabove.s3.us-west-2.amazonaws.com/a.tif",
    "https://evil.com/b.tif"
  )
  expect_error(validate_kfa_urls(bad_urls), "does not point")
})
