test_that("convert_folder errors on missing input directory", {
  expect_error(
    convert_folder("/nonexistent/dir"),
    "Input directory not found"
  )
})

test_that("convert_folder returns empty data.frame when no files found", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  result <- suppressMessages(convert_folder(tmp))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("convert_folder creates output directory if missing", {
  tmp_in  <- tempfile()
  tmp_out <- tempfile()
  dir.create(tmp_in)
  on.exit({
    unlink(tmp_in,  recursive = TRUE)
    unlink(tmp_out, recursive = TRUE)
  })

  # No files -> output dir still gets created
  suppressMessages(convert_folder(tmp_in, tmp_out))
  expect_true(dir.exists(tmp_out))
})

test_that("convert_folder skips existing output when skip_existing = TRUE", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")

  tmp_in  <- tempfile()
  tmp_out <- tempfile()
  dir.create(tmp_in)
  dir.create(tmp_out)
  on.exit({
    unlink(tmp_in,  recursive = TRUE)
    unlink(tmp_out, recursive = TRUE)
  })

  # Create a fake input mp4 and a pre-existing output
  fake_in  <- file.path(tmp_in,  "clip.mp4")
  fake_out <- file.path(tmp_out, "clip_av1.mp4")
  file.create(fake_in)
  file.create(fake_out)

  result <- suppressMessages(convert_folder(tmp_in, tmp_out, skip_existing = TRUE))
  expect_equal(nrow(result), 1L)
  expect_equal(result$status, "skipped")
})

test_that("convert_folder result has correct columns", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  result <- suppressMessages(convert_folder(tmp))
  expect_named(result, c("input", "output", "input_size", "status", "message"),
               ignore.order = TRUE)
})
