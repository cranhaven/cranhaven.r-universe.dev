## Tests for .tiff_to_png_sequence

test_that(".tiff_to_png_sequence errors without magick", {
  skip_if(requireNamespace("magick", quietly = TRUE),
          "magick is installed, cannot test missing-package path")
  tmp <- tempfile(fileext = ".tif")
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_error(
    AV1R:::.tiff_to_png_sequence(tmp, tempfile()),
    "magick"
  )
})

test_that(".tiff_to_png_sequence extracts frames from TIFF", {
  skip_if_not(requireNamespace("magick", quietly = TRUE), "magick not installed")

  # Create a minimal 3-frame TIFF via magick
  tmpdir <- tempfile("tiff_test_")
  tiff_path <- tempfile(fileext = ".tif")
  on.exit({
    unlink(tmpdir, recursive = TRUE)
    unlink(tiff_path)
  })

  frames <- c(
    magick::image_blank(8, 8, "red"),
    magick::image_blank(8, 8, "green"),
    magick::image_blank(8, 8, "blue")
  )
  magick::image_write(frames, tiff_path, format = "tiff")

  result <- suppressMessages(
    AV1R:::.tiff_to_png_sequence(tiff_path, tmpdir)
  )

  expect_true(grepl("%06d\\.png$", result))
  pngs <- list.files(tmpdir, pattern = "\\.png$")
  expect_equal(length(pngs), 3L)
})

test_that(".convert_tiff_sequence handles skip_existing", {
  skip_if_not(nchar(Sys.which("ffmpeg")) > 0, "ffmpeg not installed")

  tmp_in  <- tempfile("tiff_in_")
  tmp_out <- tempfile("tiff_out_")
  dir.create(tmp_in)
  dir.create(tmp_out)
  on.exit({
    unlink(tmp_in,  recursive = TRUE)
    unlink(tmp_out, recursive = TRUE)
  })

  # Create fake TIFF files
  for (i in 1:3) {
    file.create(file.path(tmp_in, sprintf("frame%03d.tif", i)))
  }

  # Pre-create output file
  dir_name <- basename(normalizePath(tmp_in))
  out_file <- file.path(tmp_out, paste0(dir_name, ".mp4"))
  file.create(out_file)

  opts <- av1r_options(backend = "cpu")
  tiff_files <- list.files(tmp_in, full.names = TRUE)

  result <- suppressMessages(
    AV1R:::.convert_tiff_sequence(tiff_files, tmp_in, tmp_out, opts, skip_existing = TRUE)
  )
  expect_equal(result$status, "skipped")
})
