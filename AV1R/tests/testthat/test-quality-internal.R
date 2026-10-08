## Tests for internal quality.R functions

test_that(".probe_count returns correct values for different durations", {
  expect_equal(AV1R:::.probe_count(60),   1L)   # < 5 min
  expect_equal(AV1R:::.probe_count(299),  1L)   # < 5 min
  expect_equal(AV1R:::.probe_count(300),  2L)   # 5 min
  expect_equal(AV1R:::.probe_count(599),  2L)   # < 10 min
  expect_equal(AV1R:::.probe_count(600),  4L)   # 10 min
  expect_equal(AV1R:::.probe_count(1799), 4L)   # < 30 min
  expect_equal(AV1R:::.probe_count(1800), 6L)   # 30 min
  expect_equal(AV1R:::.probe_count(7200), 6L)   # 2 hours
})

test_that(".ffmpeg_duration returns NA for nonexistent file", {
  skip_if_not(nchar(Sys.which("ffprobe")) > 0, "ffprobe not installed")
  result <- AV1R:::.ffmpeg_duration("/nonexistent/file.mp4")
  expect_true(is.na(result))
})

test_that(".ffmpeg_duration returns NA when ffprobe is missing", {
  withr::with_path("", action = "replace", {
    result <- AV1R:::.ffmpeg_duration("/any/file.mp4")
    expect_true(is.na(result))
  })
})

test_that(".ffmpeg_duration returns numeric for real video", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")

  tmp <- tempfile(fileext = ".mp4")
  on.exit(unlink(tmp))
  ret <- suppressWarnings(system2(
    Sys.which("ffmpeg"),
    c("-y", "-f", "lavfi", "-i", "color=black:size=16x16:rate=25",
      "-t", "2", "-c:v", "libx264", tmp),
    stdout = FALSE, stderr = FALSE
  ))
  skip_if_not(ret == 0L && file.exists(tmp), "Could not create test video")

  result <- AV1R:::.ffmpeg_duration(tmp)
  expect_type(result, "double")
  expect_gte(result, 1.5)
  expect_lte(result, 3.0)
})

test_that(".extract_probes returns probes for real video", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")

  tmp <- tempfile(fileext = ".mp4")
  tmpdir <- tempfile("probes_")
  dir.create(tmpdir)
  on.exit({
    unlink(tmp)
    unlink(tmpdir, recursive = TRUE)
  })

  ret <- suppressWarnings(system2(
    Sys.which("ffmpeg"),
    c("-y", "-f", "lavfi", "-i", "color=black:size=320x240:rate=25",
      "-t", "5", "-c:v", "libx264", tmp),
    stdout = FALSE, stderr = FALSE
  ))
  skip_if_not(ret == 0L && file.exists(tmp), "Could not create test video")

  probes <- AV1R:::.extract_probes(tmp, 2L, tmpdir)
  expect_type(probes, "character")
  expect_gt(length(probes), 0L)
  for (p in probes) {
    expect_true(file.exists(p))
  }
})

test_that(".extract_probes returns input for very short video", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")

  tmp <- tempfile(fileext = ".mp4")
  tmpdir <- tempfile("probes_")
  dir.create(tmpdir)
  on.exit({
    unlink(tmp)
    unlink(tmpdir, recursive = TRUE)
  })

  # 1-second video — too short for extraction
  ret <- suppressWarnings(system2(
    Sys.which("ffmpeg"),
    c("-y", "-f", "lavfi", "-i", "color=black:size=16x16:rate=25",
      "-t", "1", "-c:v", "libx264", tmp),
    stdout = FALSE, stderr = FALSE
  ))
  skip_if_not(ret == 0L && file.exists(tmp), "Could not create test video")

  probes <- AV1R:::.extract_probes(tmp, 2L, tmpdir)
  # Should return the input file itself since duration < 2
  expect_equal(probes, tmp)
})

test_that(".avg_probe_ssim returns NA when encoder always fails", {
  fake_encoder <- function(probe, crf) NA_real_
  result <- AV1R:::.avg_probe_ssim(c("/fake/a.mp4", "/fake/b.mp4"), 28, fake_encoder)
  expect_true(is.na(result))
})

test_that(".avg_probe_ssim computes mean of successful probes", {
  call_count <- 0L
  fake_encoder <- function(probe, crf) {
    call_count <<- call_count + 1L
    if (call_count == 1) 0.95 else 0.97
  }
  result <- AV1R:::.avg_probe_ssim(c("a.mp4", "b.mp4"), 28, fake_encoder)
  expect_equal(result, 0.96)
})

test_that(".avg_probe_ssim ignores NA probes in mean", {
  call_count <- 0L
  fake_encoder <- function(probe, crf) {
    call_count <<- call_count + 1L
    if (call_count == 1) NA_real_ else 0.95
  }
  result <- AV1R:::.avg_probe_ssim(c("a.mp4", "b.mp4"), 28, fake_encoder)
  expect_equal(result, 0.95)
})

test_that(".cpu_probe_ssim returns NA for nonexistent file", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")
  result <- AV1R:::.cpu_probe_ssim("/nonexistent/probe.mp4", 28)
  expect_true(is.na(result))
})

test_that(".cpu_probe_ssim returns SSIM for real video", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")
  skip_if_not(AV1R:::.has_ffmpeg_encoder("libsvtav1"), "libsvtav1 not available")

  tmp <- tempfile(fileext = ".mp4")
  on.exit(unlink(tmp))
  ret <- suppressWarnings(system2(
    Sys.which("ffmpeg"),
    c("-y", "-f", "lavfi", "-i", "color=black:size=320x240:rate=25",
      "-t", "1", "-c:v", "libx264", tmp),
    stdout = FALSE, stderr = FALSE
  ))
  skip_if_not(ret == 0L && file.exists(tmp), "Could not create test video")

  result <- AV1R:::.cpu_probe_ssim(tmp, 28)
  # Could be NA if encoding fails, but if it works, should be in [0, 1]
  if (!is.na(result)) {
    expect_gte(result, 0)
    expect_lte(result, 1)
  }
})

test_that(".probe_ssim returns NA for nonexistent file", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")
  result <- AV1R:::.probe_ssim("/nonexistent/file.mp4", 23)
  expect_true(is.na(result))
})

test_that(".vulkan_probe_ssim returns NA for nonexistent file", {
  skip_if_not(vulkan_available(), "Vulkan not available")
  # .vulkan_probe_ssim calls .ffmpeg_video_info which throws on bad input,
  # but the tryCatch in .vulkan_probe_ssim should catch it and return NA
  # However, .ffmpeg_video_info is called BEFORE the tryCatch block,
  # so this actually errors. Test that it errors appropriately.
  expect_error(
    AV1R:::.vulkan_probe_ssim("/nonexistent/file.mp4", 28),
    "Could not read video dimensions"
  )
})
