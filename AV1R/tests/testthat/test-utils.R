test_that("check_ffmpeg returns path when ffmpeg is available", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")
  result <- AV1R:::check_ffmpeg()
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("check_ffmpeg errors when ffmpeg is missing", {
  # Temporarily hide ffmpeg by overriding PATH
  withr::with_path("", action = "replace", {
    expect_error(AV1R:::check_ffmpeg(), "ffmpeg not found")
  })
})

test_that("fmt_bytes formats bytes correctly", {
  expect_equal(AV1R:::fmt_bytes(0), "0.0 B")
  expect_equal(AV1R:::fmt_bytes(512), "512.0 B")
  expect_equal(AV1R:::fmt_bytes(1024), "1.0 KB")
  expect_equal(AV1R:::fmt_bytes(1536), "1.5 KB")
  expect_equal(AV1R:::fmt_bytes(1024^2), "1.0 MB")
  expect_equal(AV1R:::fmt_bytes(1024^3), "1.0 GB")
  expect_equal(AV1R:::fmt_bytes(1024^4), "1.0 TB")
})

test_that("fmt_bytes handles large values", {
  expect_equal(AV1R:::fmt_bytes(2.5 * 1024^3), "2.5 GB")
})
