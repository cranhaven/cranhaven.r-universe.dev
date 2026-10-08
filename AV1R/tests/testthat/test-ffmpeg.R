test_that(".ffmpeg_video_info errors on missing file", {
  skip_if_not(nchar(Sys.which("ffprobe")) > 0 || nchar(Sys.which("ffmpeg")) > 0,
              "ffprobe/ffmpeg not installed")
  expect_error(
    AV1R:::.ffmpeg_video_info("/nonexistent/file.mp4"),
    "Could not read video dimensions"
  )
})

test_that(".ffmpeg_video_info returns correct structure on real video", {
  skip_if_not(nchar(Sys.which("ffprobe")) > 0 || nchar(Sys.which("ffmpeg")) > 0,
              "ffprobe/ffmpeg not installed")

  # Create a minimal valid MP4 via ffmpeg (1 second, 16x16, black)
  tmp <- tempfile(fileext = ".mp4")
  on.exit(unlink(tmp))
  ret <- suppressWarnings(system2(
    Sys.which("ffmpeg"),
    c("-y", "-f", "lavfi", "-i", "color=black:size=16x16:rate=25",
      "-t", "1", "-c:v", "libx264", tmp),
    stdout = FALSE, stderr = FALSE
  ))
  skip_if_not(ret == 0L && file.exists(tmp), "Could not create test video")

  info <- AV1R:::.ffmpeg_video_info(tmp)

  expect_type(info, "list")
  expect_named(info, c("width", "height", "fps"))
  expect_gt(info$width,  0L)
  expect_gt(info$height, 0L)
  expect_gt(info$fps,    0L)
})

test_that(".ffmpeg_video_info returns correct dimensions", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")

  tmp <- tempfile(fileext = ".mp4")
  on.exit(unlink(tmp))
  ret <- suppressWarnings(system2(
    Sys.which("ffmpeg"),
    c("-y", "-f", "lavfi", "-i", "color=blue:size=320x240:rate=30",
      "-t", "1", "-c:v", "libx264", tmp),
    stdout = FALSE, stderr = FALSE
  ))
  skip_if_not(ret == 0L && file.exists(tmp), "Could not create test video")

  info <- AV1R:::.ffmpeg_video_info(tmp)
  expect_equal(info$width, 320L)
  expect_equal(info$height, 240L)
  expect_equal(info$fps, 30L)
})

test_that(".ffmpeg_video_bitrate returns NA for nonexistent file", {
  skip_if_not(nchar(Sys.which("ffprobe")) > 0, "ffprobe not installed")
  result <- AV1R:::.ffmpeg_video_bitrate("/nonexistent/file.mp4")
  expect_true(is.na(result))
})

test_that(".ffmpeg_video_bitrate returns integer or NA for real video", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")

  tmp <- tempfile(fileext = ".mp4")
  on.exit(unlink(tmp))
  ret <- suppressWarnings(system2(
    Sys.which("ffmpeg"),
    c("-y", "-f", "lavfi", "-i", "color=black:size=320x240:rate=25",
      "-t", "2", "-c:v", "libx264", "-b:v", "500k", tmp),
    stdout = FALSE, stderr = FALSE
  ))
  skip_if_not(ret == 0L && file.exists(tmp), "Could not create test video")

  result <- AV1R:::.ffmpeg_video_bitrate(tmp)
  # Could be NA if ffprobe doesn't report bitrate for short clips
  expect_true(is.na(result) || (is.integer(result) && result > 0))
})

test_that(".ffmpeg_video_bitrate returns NA when ffprobe is missing", {
  withr::with_path("", action = "replace", {
    result <- AV1R:::.ffmpeg_video_bitrate("/some/file.mp4")
    expect_true(is.na(result))
  })
})
