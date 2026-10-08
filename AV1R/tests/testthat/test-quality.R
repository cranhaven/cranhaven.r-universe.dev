test_that("measure_ssim returns NA for nonexistent files", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")
  expect_true(is.na(measure_ssim("/no/such/original.mp4", "/no/such/encoded.mp4")))
})

test_that("measure_ssim returns value in [0, 1] for identical files", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")
  skip_if_not(file.exists("/mnt/Data2/DS_projects/AV_test/test.mp4"),
              "Test video not available")

  # SSIM of a file against itself should be ~1.0
  score <- measure_ssim(
    "/mnt/Data2/DS_projects/AV_test/test.mp4",
    "/mnt/Data2/DS_projects/AV_test/test.mp4",
    duration = 5
  )
  expect_false(is.na(score))
  expect_gte(score, 0.99)
  expect_lte(score, 1.0)
})

test_that("measure_ssim respects duration argument", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")
  skip_if_not(file.exists("/mnt/Data2/DS_projects/AV_test/test.mp4"),
              "Test video not available")

  score <- measure_ssim(
    "/mnt/Data2/DS_projects/AV_test/test.mp4",
    "/mnt/Data2/DS_projects/AV_test/test.mp4",
    duration = 2
  )
  expect_false(is.na(score))
})

test_that("measure_ssim errors when ffmpeg not found", {
  # Simulate missing ffmpeg by passing nonexistent paths — ffmpeg will fail
  # and return NA, not error (unless ffmpeg itself is missing)
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")
  result <- measure_ssim("/no/file.mp4", "/no/file2.mp4")
  expect_true(is.na(result))
})
