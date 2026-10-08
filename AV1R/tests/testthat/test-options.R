test_that("av1r_options returns correct defaults", {
  o <- av1r_options()
  expect_equal(o$crf,     28L)
  expect_equal(o$preset,   8L)
  expect_equal(o$threads,  0L)
  expect_equal(o$backend, "auto")
  expect_null(o$bitrate)
  expect_s3_class(o, "av1r_options")
})

test_that("av1r_options accepts bitrate parameter", {
  expect_no_error(av1r_options(bitrate = 3000))
  expect_equal(av1r_options(bitrate = 3000)$bitrate, 3000L)
  expect_error(av1r_options(bitrate = 0))
  expect_error(av1r_options(bitrate = -1))
})

test_that("av1r_options validates crf range", {
  expect_error(av1r_options(crf = -1))
  expect_error(av1r_options(crf = 64))
  expect_no_error(av1r_options(crf = 0))
  expect_no_error(av1r_options(crf = 63))
})

test_that("av1r_options validates preset range", {
  expect_error(av1r_options(preset = -1))
  expect_error(av1r_options(preset = 14))
  expect_no_error(av1r_options(preset = 0))
  expect_no_error(av1r_options(preset = 13))
})

test_that("av1r_options validates backend", {
  expect_no_error(av1r_options(backend = "auto"))
  expect_no_error(av1r_options(backend = "cpu"))
  expect_no_error(av1r_options(backend = "vulkan"))
  expect_no_error(av1r_options(backend = "vaapi"))
  expect_error(av1r_options(backend = "gpu"))
  expect_error(av1r_options(backend = "invalid"))
})

test_that("av1r_options validates tiff_crf range", {
  expect_error(av1r_options(tiff_crf = -1))
  expect_error(av1r_options(tiff_crf = 64))
  expect_no_error(av1r_options(tiff_crf = 0))
  expect_no_error(av1r_options(tiff_crf = 63))
})

test_that("av1r_options default tiff_crf is 5", {
  o <- av1r_options()
  expect_equal(o$tiff_crf, 5L)
})

test_that("av1r_options validates tiff_scale", {
  expect_no_error(av1r_options(tiff_scale = 2))
  expect_no_error(av1r_options(tiff_scale = c(800, 600)))
  expect_error(av1r_options(tiff_scale = 0))
  expect_error(av1r_options(tiff_scale = -1))
  expect_error(av1r_options(tiff_scale = c(1, 2, 3)))
})

test_that("av1r_options validates verbose", {
  expect_no_error(av1r_options(verbose = TRUE))
  expect_no_error(av1r_options(verbose = FALSE))
  expect_error(av1r_options(verbose = "yes"))
  expect_error(av1r_options(verbose = 1))
})

test_that("av1r_options stores all parameters correctly", {
  o <- av1r_options(crf = 10, preset = 4, threads = 8, bitrate = 5000,
                    tiff_crf = 3, tiff_scale = c(1024, 768),
                    verbose = FALSE, backend = "cpu")
  expect_equal(o$crf, 10L)
  expect_equal(o$preset, 4L)
  expect_equal(o$threads, 8L)
  expect_equal(o$bitrate, 5000L)
  expect_equal(o$tiff_crf, 3L)
  expect_equal(o$tiff_scale, c(1024, 768))
  expect_false(o$verbose)
  expect_equal(o$backend, "cpu")
})

test_that("print.av1r_options prints without error", {
  o <- av1r_options()
  expect_output(print(o))
})

test_that("print.av1r_options shows tiff_scale", {
  o1 <- av1r_options(tiff_scale = 3)
  expect_output(print(o1), "tiff_scale=3x")

  o2 <- av1r_options(tiff_scale = c(800, 600))
  expect_output(print(o2), "tiff_scale=800x600")
})

test_that("print.av1r_options returns invisibly", {
  o <- av1r_options()
  out <- capture.output(res <- print(o))
  expect_identical(res, o)
})
