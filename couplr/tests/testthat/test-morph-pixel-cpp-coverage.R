# ==============================================================================
# Tests to increase morph pixel C++ coverage via high-level interface
# ==============================================================================

# ------------------------------------------------------------------------------
# Basic pixel morphing tests
# ------------------------------------------------------------------------------

test_that("pixel_morph works with exact mode on tiny images", {
  skip_on_cran()
  skip_if_not_installed("magick")
  skip_if_not_installed("png")

  # Create tiny 2x2 images
  imgA <- magick::image_blank(2, 2, color = "red")
  imgB <- magick::image_blank(2, 2, color = "blue")

  result <- pixel_morph(imgA, imgB, n_frames = 3, mode = "exact")

  expect_s3_class(result, "magick-image")
  expect_true(length(result) >= 1)
})

test_that("pixel_morph works with color_walk mode", {
  skip_on_cran()
  skip_if_not_installed("magick")
  skip_if_not_installed("png")

  imgA <- magick::image_blank(4, 4, color = "red")
  imgB <- magick::image_blank(4, 4, color = "blue")

  result <- pixel_morph(imgA, imgB, n_frames = 3, mode = "color_walk",
                        quantize_bits = 3)

  expect_s3_class(result, "magick-image")
})

test_that("pixel_morph works with recursive mode", {
  skip_on_cran()
  skip_if_not_installed("magick")
  skip_if_not_installed("png")

  imgA <- magick::image_blank(6, 6, color = "red")
  imgB <- magick::image_blank(6, 6, color = "blue")

  result <- pixel_morph(imgA, imgB, n_frames = 3, mode = "recursive")

  expect_s3_class(result, "magick-image")
})

# ------------------------------------------------------------------------------
# Edge cases for pixel morphing
# ------------------------------------------------------------------------------

test_that("pixel_morph handles grayscale images", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(3, 3, color = "gray50")
  imgB <- magick::image_blank(3, 3, color = "gray80")

  result <- pixel_morph(imgA, imgB, n_frames = 2, mode = "exact")

  expect_s3_class(result, "magick-image")
})

test_that("pixel_morph handles single frame with warning", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(3, 3, color = "red")
  imgB <- magick::image_blank(3, 3, color = "blue")

  expect_warning(
    result <- pixel_morph(imgA, imgB, n_frames = 1, mode = "exact"),
    "n_frames must be at least 2"
  )

  expect_s3_class(result, "magick-image")
})

test_that("pixel_morph handles many frames", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(3, 3, color = "red")
  imgB <- magick::image_blank(3, 3, color = "blue")

  result <- pixel_morph(imgA, imgB, n_frames = 10, mode = "exact")

  expect_s3_class(result, "magick-image")
})

# ------------------------------------------------------------------------------
# Different image sizes and aspect ratios
# ------------------------------------------------------------------------------

test_that("pixel_morph handles wide images", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(6, 2, color = "red")
  imgB <- magick::image_blank(6, 2, color = "blue")

  result <- pixel_morph(imgA, imgB, n_frames = 2, mode = "exact")

  expect_s3_class(result, "magick-image")
})

test_that("pixel_morph handles tall images", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(2, 6, color = "red")
  imgB <- magick::image_blank(2, 6, color = "blue")

  result <- pixel_morph(imgA, imgB, n_frames = 2, mode = "exact")

  expect_s3_class(result, "magick-image")
})

test_that("pixel_morph handles odd dimensions", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(3, 5, color = "red")
  imgB <- magick::image_blank(3, 5, color = "blue")

  result <- pixel_morph(imgA, imgB, n_frames = 2, mode = "recursive")

  expect_s3_class(result, "magick-image")
})

# ------------------------------------------------------------------------------
# Alpha and beta parameter tests
# ------------------------------------------------------------------------------

test_that("pixel_morph alpha and beta parameters work", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(3, 3, color = "red")
  imgB <- magick::image_blank(3, 3, color = "blue")

  # High alpha = color matters more
  result1 <- pixel_morph(imgA, imgB, n_frames = 2, mode = "exact",
                         alpha = 2, beta = 0.1)

  # High beta = position matters more
  result2 <- pixel_morph(imgA, imgB, n_frames = 2, mode = "exact",
                         alpha = 0.1, beta = 2)

  expect_s3_class(result1, "magick-image")
  expect_s3_class(result2, "magick-image")
})

# ------------------------------------------------------------------------------
# Different lap methods
# ------------------------------------------------------------------------------

test_that("pixel_morph works with different lap_method", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(3, 3, color = "red")
  imgB <- magick::image_blank(3, 3, color = "blue")

  result <- pixel_morph(imgA, imgB, n_frames = 2, mode = "exact",
                        lap_method = "hungarian")

  expect_s3_class(result, "magick-image")
})

test_that("pixel_morph works with auction method", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(3, 3, color = "red")
  imgB <- magick::image_blank(3, 3, color = "blue")

  result <- pixel_morph(imgA, imgB, n_frames = 2, mode = "exact",
                        lap_method = "auction")

  expect_s3_class(result, "magick-image")
})

# ------------------------------------------------------------------------------
# maximize parameter
# ------------------------------------------------------------------------------

test_that("pixel_morph works with maximize = TRUE", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(3, 3, color = "red")
  imgB <- magick::image_blank(3, 3, color = "blue")

  result <- pixel_morph(imgA, imgB, n_frames = 2, mode = "exact",
                        maximize = TRUE)

  expect_s3_class(result, "magick-image")
})

# ------------------------------------------------------------------------------
# downscale_steps parameter
# ------------------------------------------------------------------------------

test_that("pixel_morph with downscale_steps", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(8, 8, color = "red")
  imgB <- magick::image_blank(8, 8, color = "blue")

  result <- pixel_morph(imgA, imgB, n_frames = 2, mode = "exact",
                        downscale_steps = 1)

  expect_s3_class(result, "magick-image")
})

# ------------------------------------------------------------------------------
# patch_size parameter
# ------------------------------------------------------------------------------

test_that("pixel_morph with patch_size", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(6, 6, color = "red")
  imgB <- magick::image_blank(6, 6, color = "blue")

  result <- pixel_morph(imgA, imgB, n_frames = 2, mode = "exact",
                        patch_size = 2)

  expect_s3_class(result, "magick-image")
})

# ------------------------------------------------------------------------------
# quantize_bits parameter
# ------------------------------------------------------------------------------

test_that("pixel_morph with different quantize_bits", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(4, 4, color = "red")
  imgB <- magick::image_blank(4, 4, color = "blue")

  result1 <- pixel_morph(imgA, imgB, n_frames = 2, mode = "color_walk",
                         quantize_bits = 2)

  result2 <- pixel_morph(imgA, imgB, n_frames = 2, mode = "color_walk",
                         quantize_bits = 6)

  expect_s3_class(result1, "magick-image")
  expect_s3_class(result2, "magick-image")
})

# ------------------------------------------------------------------------------
# Testing with actual RGB arrays
# ------------------------------------------------------------------------------

test_that("pixel_morph with RGB array input", {
  skip_on_cran()
  skip_if_not_installed("magick")
  skip_if_not_installed("png")

  # Create 3x3 RGB images
  A <- array(c(rep(1, 9), rep(0, 9), rep(0, 9)), dim = c(3, 3, 3))
  B <- array(c(rep(0, 9), rep(1, 9), rep(0, 9)), dim = c(3, 3, 3))

  imgA <- magick::image_read(A)
  imgB <- magick::image_read(B)

  result <- pixel_morph(imgA, imgB, n_frames = 3, mode = "exact")

  expect_s3_class(result, "magick-image")
})

# ------------------------------------------------------------------------------
# Color walk specific parameters
# ------------------------------------------------------------------------------

test_that("pixel_morph color_walk with various settings", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(4, 4, color = "#FF0000")
  imgB <- magick::image_blank(4, 4, color = "#0000FF")

  result <- pixel_morph(imgA, imgB, n_frames = 4, mode = "color_walk",
                        quantize_bits = 4, alpha = 1.5, beta = 0.5)

  expect_s3_class(result, "magick-image")
})

# ------------------------------------------------------------------------------
# Recursive mode specific tests
# ------------------------------------------------------------------------------

test_that("pixel_morph recursive with larger images", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- magick::image_blank(8, 8, color = "red")
  imgB <- magick::image_blank(8, 8, color = "blue")

  result <- pixel_morph(imgA, imgB, n_frames = 3, mode = "recursive")

  expect_s3_class(result, "magick-image")
})

# ------------------------------------------------------------------------------
# Internal C++ function spatial_cost_matrix_cpp
# ------------------------------------------------------------------------------

test_that("spatial_cost_matrix_cpp works", {
  skip_on_cran()
  H <- 3
  W <- 3
  idxA <- as.integer(c(0, 1, 2))  # 0-indexed
  idxB <- as.integer(c(3, 4, 5))

  result <- couplr:::spatial_cost_matrix_cpp(idxA, idxB, H, W)

  expect_equal(dim(result), c(3, 3))
  expect_true(is.numeric(result))
})

test_that("spatial_cost_matrix_cpp handles identical indices", {
  skip_on_cran()
  H <- 2
  W <- 2
  idx <- as.integer(c(0, 1))

  result <- couplr:::spatial_cost_matrix_cpp(idx, idx, H, W)

  # Diagonal should be 0 (same position)
  expect_equal(result[1, 1], 0)
  expect_equal(result[2, 2], 0)
})

# ------------------------------------------------------------------------------
# compute_pixel_cost_cpp
# ------------------------------------------------------------------------------

test_that("compute_pixel_cost_cpp works", {
  skip_on_cran()
  H <- 2
  W <- 2
  N <- H * W
  pixelsA <- rep(0.5, N * 3)
  pixelsB <- rep(0.5, N * 3)

  result <- couplr:::compute_pixel_cost_cpp(pixelsA, pixelsB, H, W, alpha = 1, beta = 0.1)

  expect_equal(dim(result), c(N, N))
  expect_true(all(is.finite(result)))
})

# ------------------------------------------------------------------------------
# downscale_image_cpp
# ------------------------------------------------------------------------------

test_that("downscale_image_cpp works", {
  skip_on_cran()
  H <- 4
  W <- 4
  N <- H * W
  pixels <- rep(0.5, N * 3)

  result <- couplr:::downscale_image_cpp(pixels, H, W, 2L, 2L)

  expect_equal(length(result), 2 * 2 * 3)
})

# ------------------------------------------------------------------------------
# upscale_assignment_cpp
# ------------------------------------------------------------------------------

test_that("upscale_assignment_cpp works", {
  skip_on_cran()
  # 2x2 assignment (0-indexed)
  assignment <- as.integer(c(0, 1, 2, 3))

  result <- couplr:::upscale_assignment_cpp(assignment, 4L, 4L, 2L, 2L)

  expect_equal(length(result), 16)  # 4x4 output
})
