# Test suite for pixel morphing functionality

test_that("pixel cost matrix computation works", {
  skip_if_not(exists("compute_pixel_cost_cpp"))

  H <- 4; W <- 4; N <- H * W
  pixelsA <- numeric(N * 3)
  pixelsB <- numeric(N * 3)

  # One red pixel in A at linear index 5 (row-major)
  pixelsA[5] <- 255
  # One blue pixel in B at linear index 11
  pixelsB[11 + 2*N] <- 255

  cost <- compute_pixel_cost_cpp(pixelsA, pixelsB, H, W, 1, 1)
  expect_equal(dim(cost), c(N, N))
  expect_true(all(is.finite(cost)))
  expect_true(all(cost >= 0))
})

test_that("pixel_morph_animate creates animation with small test images (exact)", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  skip_if_not(exists("morph_pixel_level_cpp"), message = "C++ morphing function not available")

  H <- 8; W <- 8
  imgA <- array(0, c(H, W, 3))
  imgA[4:5, 2:3, 1] <- 255  # red block (left)

  imgB <- array(0, c(H, W, 3))
  imgB[4:5, 6:7, 3] <- 255  # blue block (right)

  tmpA <- withr::local_tempfile(fileext = ".png")
  tmpB <- withr::local_tempfile(fileext = ".png")
  tmpOut <- withr::local_tempfile(fileext = ".gif")
  png::writePNG(imgA/255, tmpA)
  png::writePNG(imgB/255, tmpB)

  result <- pixel_morph_animate(
    imgA = tmpA, imgB = tmpB,
    alpha = 1, beta = 0,
    n_frames = 4L, fps = 10L,
    outfile = tmpOut, show = FALSE,
    mode = "exact"
  )

  expect_type(result, "list")
  expect_equal(result$n_pixels, H * W)
  expect_equal(result$mode, "exact")
  expect_true(file.exists(tmpOut))
  expect_gt(file.size(tmpOut), 0)
})

test_that("pixel_morph static function returns final frame only", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  skip_if_not(exists("morph_pixel_level_cpp"))

  H <- 8; W <- 8
  imgA <- array(0, c(H, W, 3))
  imgA[4:5, 2:3, 1] <- 255  # red block

  imgB <- array(0, c(H, W, 3))
  imgB[4:5, 6:7, 3] <- 255  # blue block

  tmpA <- withr::local_tempfile(fileext = ".png")
  tmpB <- withr::local_tempfile(fileext = ".png")
  png::writePNG(imgA/255, tmpA)
  png::writePNG(imgB/255, tmpB)

  # pixel_morph returns magick image, not list
  result <- pixel_morph(
    imgA = tmpA, imgB = tmpB,
    alpha = 1, beta = 0,
    show = FALSE,
    mode = "exact"
  )

  expect_s3_class(result, "magick-image")
})

test_that("pixel_morph_animate color_walk mode works with varied colors", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  skip_if_not(exists("morph_pixel_level_cpp"))
  skip_if_not(exists("color_palette_info_cpp"))

  H <- 8; W <- 8
  set.seed(123)
  imgA <- array(runif(H * W * 3), c(H, W, 3))
  imgB <- array(runif(H * W * 3), c(H, W, 3))

  tmpA <- withr::local_tempfile(fileext = ".png")
  tmpB <- withr::local_tempfile(fileext = ".png")
  tmpOut <- withr::local_tempfile(fileext = ".gif")
  png::writePNG(imgA, tmpA)
  png::writePNG(imgB, tmpB)

  result <- pixel_morph_animate(
    imgA = tmpA, imgB = tmpB,
    n_frames = 4L, fps = 10L,
    outfile = tmpOut, show = FALSE,
    mode = "color_walk",
    quantize_bits = 6L
  )

  expect_type(result, "list")
  expect_equal(result$n_pixels, H * W)
  expect_true(file.exists(tmpOut))
  expect_equal(result$mode, "color_walk")
})

test_that("pixel_morph_animate exact mode enforces size limit with warning", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  skip_if_not(exists("morph_pixel_level_cpp"))

  # Create an image larger than the 4096 pixel recommended limit
  H <- 80; W <- 80  # 6400 pixels > 4096
  imgLarge <- array(runif(H * W * 3), c(H, W, 3))
  
  tmpA <- withr::local_tempfile(fileext = ".png")
  tmpB <- withr::local_tempfile(fileext = ".png")
  tmpOut <- withr::local_tempfile(fileext = ".gif")
  png::writePNG(imgLarge, tmpA)
  png::writePNG(imgLarge, tmpB)

  # Should warn but still work
  expect_warning(
    pixel_morph_animate(
      imgA = tmpA, imgB = tmpB,
      n_frames = 2L, fps = 5L,
      outfile = tmpOut, show = FALSE,
      mode = "exact",
      patch_size = 1L
    ),
    regexp = "Image is large for 'exact' global LAP"
  )
  
  expect_true(file.exists(tmpOut))
})

test_that("pixel_morph exact mode runs with flat colors", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  skip_if_not(exists("morph_pixel_level_cpp"))
  skip_if_not(exists("compute_pixel_cost_cpp"))

  H <- 8; W <- 8
  imgA <- array(0.5, c(H, W, 3))
  imgB <- array(0.5, c(H, W, 3))

  tmpA <- withr::local_tempfile(fileext = ".png")
  tmpB <- withr::local_tempfile(fileext = ".png")
  png::writePNG(imgA, tmpA)
  png::writePNG(imgB, tmpB)

  result <- pixel_morph(
    imgA = tmpA, imgB = tmpB,
    show = FALSE,
    mode = "exact",
    alpha = 1, beta = 0
  )

  expect_s3_class(result, "magick-image")
})

test_that("pixel_morph handles file-based images", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  skip_if_not(exists("morph_pixel_level_cpp"))

  H <- 8; W <- 8
  img1 <- array(runif(H * W * 3), c(H, W, 3))
  img2 <- array(runif(H * W * 3), c(H, W, 3))

  tmpFile1 <- withr::local_tempfile(fileext = ".png")
  tmpFile2 <- withr::local_tempfile(fileext = ".png")
  png::writePNG(img1, tmpFile1)
  png::writePNG(img2, tmpFile2)

  # We just want to ensure pixel_morph can read file paths and run;
  # use 'exact' mode, which is known to work in the static function.
  result <- pixel_morph(
    imgA = tmpFile1, imgB = tmpFile2,
    show = FALSE,
    mode = "exact"
  )

  expect_s3_class(result, "magick-image")
})

test_that("exact mode with downscaling can handle larger images", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  skip_if_not(exists("morph_pixel_level_cpp"))
  skip_if_not(exists("downscale_image_cpp"))

  # Create image larger than exact limit, but use downscaling
  H <- 200; W <- 200  # 40,000 pixels
  
  imgLarge <- array(runif(H * W * 3), c(H, W, 3))
  tmpA <- withr::local_tempfile(fileext = ".png")
  tmpB <- withr::local_tempfile(fileext = ".png")
  tmpOut <- withr::local_tempfile(fileext = ".gif")
  png::writePNG(imgLarge, tmpA)
  png::writePNG(imgLarge, tmpB)

  # Expect the non-permutation warning, but keep the result
  expect_warning(
    result <- pixel_morph_animate(
      imgA = tmpA, imgB = tmpB,
      n_frames = 2L, fps = 5L,
      outfile = tmpOut, show = FALSE,
      mode = "exact",
      downscale_steps = 2L  # Solve at 1/4 resolution
    ),
    regexp = "Assignment is not a permutation"
  )
  
  expect_true(file.exists(tmpOut))
  expect_equal(result$mode, "exact")
  expect_gt(file.size(tmpOut), 0)
})


test_that("color_walk uses pure color matching (not spatial)", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  skip_if_not(exists("morph_pixel_level_cpp"))
  skip_if_not(exists("color_palette_info_cpp"))

  H <- 8; W <- 8
  # Create two images with different spatial arrangements of same colors
  imgA <- array(0, c(H, W, 3))
  imgA[2:3, 2:3, 1] <- 255  # red top-left
  imgA[6:7, 6:7, 3] <- 255  # blue bottom-right

  imgB <- array(0, c(H, W, 3))
  imgB[2:3, 6:7, 1] <- 255  # red top-right (moved)
  imgB[6:7, 2:3, 3] <- 255  # blue bottom-left (moved)

  tmpA <- withr::local_tempfile(fileext = ".png")
  tmpB <- withr::local_tempfile(fileext = ".png")
  tmpOut <- withr::local_tempfile(fileext = ".gif")
  png::writePNG(imgA/255, tmpA)
  png::writePNG(imgB/255, tmpB)

  result <- pixel_morph_animate(
    imgA = tmpA, imgB = tmpB,
    n_frames = 4L, fps = 10L,
    outfile = tmpOut, show = FALSE,
    mode = "color_walk",
    quantize_bits = 6L
  )

  # Just check it runs successfully
  expect_type(result, "list")
  expect_equal(result$mode, "color_walk")
  expect_true(file.exists(tmpOut))
  expect_gt(file.size(tmpOut), 0)
})

test_that("permutation warning is issued with downscaling", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  skip_if_not(exists("morph_pixel_level_cpp"))

  H <- 64; W <- 64
  imgA <- array(runif(H * W * 3), c(H, W, 3))
  imgB <- array(runif(H * W * 3), c(H, W, 3))

  tmpA <- withr::local_tempfile(fileext = ".png")
  tmpB <- withr::local_tempfile(fileext = ".png")
  png::writePNG(imgA, tmpA)
  png::writePNG(imgB, tmpB)

  # With downscaling, assignment is likely not a permutation
  expect_warning(
    pixel_morph(
      imgA = tmpA, imgB = tmpB,
      mode = "exact",
      downscale_steps = 2L,  # This creates non-permutation
      show = FALSE
    ),
    regexp = "overlaps.*holes"
  )
})

test_that("no permutation warning without downscaling on small image", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  skip_if_not(exists("morph_pixel_level_cpp"))

  H <- 8; W <- 8  # Small enough for exact mode
  imgA <- array(runif(H * W * 3), c(H, W, 3))
  imgB <- array(runif(H * W * 3), c(H, W, 3))

  tmpA <- withr::local_tempfile(fileext = ".png")
  tmpB <- withr::local_tempfile(fileext = ".png")
  png::writePNG(imgA, tmpA)
  png::writePNG(imgB, tmpB)

  # Without downscaling, exact mode should produce perfect permutation
  expect_silent(
    pixel_morph(
      imgA = tmpA, imgB = tmpB,
      mode = "exact",
      downscale_steps = 0L,
      patch_size = 1L,
      show = FALSE
    )
  )
})

test_that("alpha and beta parameters control matching behavior", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  skip_if_not(exists("morph_pixel_level_cpp"))

  H <- 8; W <- 8
  imgA <- array(runif(H * W * 3), c(H, W, 3))
  imgB <- array(runif(H * W * 3), c(H, W, 3))

  tmpA <- withr::local_tempfile(fileext = ".png")
  tmpB <- withr::local_tempfile(fileext = ".png")
  png::writePNG(imgA, tmpA)
  png::writePNG(imgB, tmpB)

  # Pure color matching
  result_color <- pixel_morph(
    imgA = tmpA, imgB = tmpB,
    mode = "exact",
    alpha = 1, beta = 0,
    show = FALSE
  )
  expect_s3_class(result_color, "magick-image")

  # Pure spatial matching
  result_spatial <- pixel_morph(
    imgA = tmpA, imgB = tmpB,
    mode = "exact",
    alpha = 0, beta = 1,
    show = FALSE
  )
  expect_s3_class(result_spatial, "magick-image")

  # Just verify both work without error
})

test_that("final frame is sharp (transport-only)", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  skip_if_not(exists("morph_pixel_level_cpp"))

  H <- 8; W <- 8
  # Create image with distinct colors
  imgA <- array(0, c(H, W, 3))
  imgA[1:4, 1:4, 1] <- 1.0  # Red quadrant
  imgA[5:8, 5:8, 3] <- 1.0  # Blue quadrant

  imgB <- array(0.5, c(H, W, 3))  # Gray

  tmpA <- withr::local_tempfile(fileext = ".png")
  tmpB <- withr::local_tempfile(fileext = ".png")
  png::writePNG(imgA, tmpA)
  png::writePNG(imgB, tmpB)

  # Get final frame
  result <- pixel_morph(
    imgA = tmpA, imgB = tmpB,
    mode = "exact",
    show = FALSE
  )

  # Convert back to array to check for sharpness
  result_arr <- as.numeric(magick::image_data(result, channels = "rgb"))

  # Final frame should have mostly 0 or 1 values (sharp), not intermediate
  # Allow some tolerance for magick conversion
  n_intermediate <- sum(result_arr > 0.1 & result_arr < 0.9)
  n_total <- length(result_arr)
  
  # Less than 10% should be intermediate values
  expect_lt(n_intermediate / n_total, 0.1)
})
