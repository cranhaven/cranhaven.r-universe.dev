## Tests for .ffmpeg_encode_av1 and .vaapi_encode_av1

test_that(".ffmpeg_encode_av1 encodes a minimal video", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")
  skip_if_not(AV1R:::.has_ffmpeg_encoder("libsvtav1"), "libsvtav1 not available")

  tmp_in  <- tempfile(fileext = ".mp4")
  tmp_out <- tempfile(fileext = ".mp4")
  on.exit(unlink(c(tmp_in, tmp_out)))

  ret <- suppressWarnings(system2(
    Sys.which("ffmpeg"),
    c("-y", "-f", "lavfi", "-i", "color=red:size=320x240:rate=25",
      "-t", "1", "-c:v", "libx264", tmp_in),
    stdout = FALSE, stderr = FALSE
  ))
  skip_if_not(ret == 0L && file.exists(tmp_in), "Could not create test video")

  opts <- av1r_options(crf = 40, preset = 12, backend = "cpu")
  expect_no_error(
    suppressMessages(AV1R:::.ffmpeg_encode_av1(tmp_in, tmp_out, opts))
  )
  expect_true(file.exists(tmp_out))
  expect_gt(file.info(tmp_out)$size, 0)
})

test_that(".ffmpeg_encode_av1 errors on nonexistent input", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")
  skip_if_not(AV1R:::.has_ffmpeg_encoder("libsvtav1"), "libsvtav1 not available")

  opts <- av1r_options(crf = 28, backend = "cpu")
  expect_error(
    suppressMessages(AV1R:::.ffmpeg_encode_av1("/no/file.mp4", tempfile(), opts)),
    "ffmpeg failed|Could not read"
  )
})

test_that(".vaapi_encode_av1 errors when ffmpeg is missing", {
  withr::with_path("", action = "replace", {
    opts <- av1r_options(backend = "vaapi")
    expect_error(
      AV1R:::.vaapi_encode_av1("/any/file.mp4", tempfile(), opts),
      "ffmpeg not found"
    )
  })
})

test_that(".vaapi_encode_av1 encodes a minimal video", {
  skip_if_not(AV1R:::.vaapi_av1_available(), "VAAPI AV1 not available")
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")

  tmp_in  <- tempfile(fileext = ".mp4")
  tmp_out <- tempfile(fileext = ".mp4")
  on.exit(unlink(c(tmp_in, tmp_out)))

  ret <- suppressWarnings(system2(
    Sys.which("ffmpeg"),
    c("-y", "-f", "lavfi", "-i", "color=green:size=320x240:rate=25",
      "-t", "1", "-c:v", "libx264", tmp_in),
    stdout = FALSE, stderr = FALSE
  ))
  skip_if_not(ret == 0L && file.exists(tmp_in), "Could not create test video")

  opts <- av1r_options(backend = "vaapi")
  expect_no_error(
    suppressMessages(AV1R:::.vaapi_encode_av1(tmp_in, tmp_out, opts))
  )
  expect_true(file.exists(tmp_out))
  expect_gt(file.info(tmp_out)$size, 0)
})
