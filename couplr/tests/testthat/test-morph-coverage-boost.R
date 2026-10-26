# ==============================================================================
# Additional coverage tests to boost morph files to 90%+
# ==============================================================================

# ------------------------------------------------------------------------------
# morph_utils.R: .call_or fallback paths (lines 131-138)
# ------------------------------------------------------------------------------

test_that(".call_or uses fallback when primary doesn't exist", {
  skip_on_cran()
  # Create test function in global env temporarily
  test_fallback_fn <- function(x) x * 2
  assign("test_fallback_fn", test_fallback_fn, envir = .GlobalEnv)

  result <- couplr:::.call_or(
    "nonexistent_primary_xyz123",
    "test_fallback_fn",
    5
  )
  expect_equal(result, 10)

  rm("test_fallback_fn", envir = .GlobalEnv)
})

test_that(".call_or errors when neither function exists", {
  skip_on_cran()
  expect_error(
    couplr:::.call_or("nonexistent_a_xyz123", "nonexistent_b_xyz123", 1),
    "Neither.*nor.*is available"
  )
})

# ------------------------------------------------------------------------------
# morph_utils.R: .lap_assign with tibble/data.frame return (lines 236-240)
# ------------------------------------------------------------------------------

test_that(".lap_assign handles data.frame with source/target", {
  skip_on_cran()
  # Temporarily override lap_solve to return data.frame
  original_fn <- couplr::lap_solve

  mock_lap_solve <- function(C, ...) {
    n <- nrow(C)
    data.frame(source = 1:n, target = n:1)  # Reverse assignment
  }

  # Use assignInNamespace to temporarily replace
  assignInNamespace("lap_solve", mock_lap_solve, ns = "couplr")

  cost <- matrix(runif(9), 3, 3)
  result <- couplr:::.lap_assign(cost, method = "jv")

  # Restore
  assignInNamespace("lap_solve", original_fn, ns = "couplr")

  expect_length(result, 3)
})

# ------------------------------------------------------------------------------
# morph_utils.R: .palette_pairs_identity edge cases (lines 350-368)
# ------------------------------------------------------------------------------

test_that(".palette_pairs_identity handles no matching colors", {
  skip_on_cran()
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0), nrow = 1, ncol = 3),
    colorsB_rgb = matrix(c(0, 255, 0), nrow = 1, ncol = 3),  # Different color
    countsA = 10L,
    countsB = 10L
  )

  result <- couplr:::.palette_pairs_identity(info)

  expect_equal(nrow(result), 0)  # No matching colors
})

test_that(".palette_pairs_identity handles exact color matches", {
  skip_on_cran()
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0, 0, 255, 0), nrow = 2, ncol = 3, byrow = TRUE),
    colorsB_rgb = matrix(c(255, 0, 0, 0, 0, 255), nrow = 2, ncol = 3, byrow = TRUE),  # First matches
    countsA = c(10L, 5L),
    countsB = c(8L, 12L)
  )

  result <- couplr:::.palette_pairs_identity(info)

  expect_equal(nrow(result), 1)  # One color matches
  expect_equal(result$ia, 1)
  expect_equal(result$ib, 1)
  expect_equal(result$k, 8)  # min(10, 8)
})

# ------------------------------------------------------------------------------
# morph_utils.R: .build_spatial_assignments_for_pairs empty pairs (line 314)
# ------------------------------------------------------------------------------

test_that(".build_spatial_assignments_for_pairs handles empty pairs", {
  skip_on_cran()
  info <- list(groupsA = list(), groupsB = list())
  pairs <- data.frame(ia = integer(0), ib = integer(0), k = integer(0))

  result <- couplr:::.build_spatial_assignments_for_pairs(info, pairs, H = 4, W = 4)

  expect_equal(length(result$i_idx), 0)
  expect_equal(length(result$j_idx), 0)
})

# ------------------------------------------------------------------------------
# morph_utils.R: .assemble_assignment edge cases (lines 332-338)
# ------------------------------------------------------------------------------

test_that(".assemble_assignment handles empty inputs", {
  skip_on_cran()
  result <- couplr:::.assemble_assignment(N = 5, i_idx = integer(0), j_idx = integer(0))
  expect_equal(result, rep(-1L, 5))
})

test_that(".assemble_assignment handles mismatched lengths", {
  skip_on_cran()
  # When i_idx and j_idx have different lengths
  result <- couplr:::.assemble_assignment(N = 5, i_idx = c(1, 2, 3), j_idx = c(5, 4))
  expect_equal(length(result), 5)
  expect_equal(result[1], 5L)
  expect_equal(result[2], 4L)
  expect_equal(result[3], -1L)  # Not assigned (no j value)
})

# ------------------------------------------------------------------------------
# morph_pixel.R: Input validation error paths
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate validates upscale", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  expect_error(
    pixel_morph_animate(imgA, imgB, upscale = NA, show = FALSE),
    "upscale must be a single numeric"
  )

  expect_error(
    pixel_morph_animate(imgA, imgB, upscale = c(1, 2), show = FALSE),
    "upscale must be a single numeric"
  )
})

test_that("pixel_morph_animate validates n_frames", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  expect_error(
    pixel_morph_animate(imgA, imgB, n_frames = NA, show = FALSE),
    "n_frames must be a single numeric"
  )
})

test_that("pixel_morph_animate validates alpha/beta", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  expect_error(
    pixel_morph_animate(imgA, imgB, alpha = -1, show = FALSE),
    "alpha must be a single non-negative"
  )

  expect_error(
    pixel_morph_animate(imgA, imgB, beta = -1, show = FALSE),
    "beta must be a single non-negative"
  )

  expect_error(
    pixel_morph_animate(imgA, imgB, alpha = 0, beta = 0, show = FALSE),
    "alpha and beta cannot both be zero"
  )
})

test_that("pixel_morph_animate validates patch_size", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  expect_error(
    pixel_morph_animate(imgA, imgB, patch_size = NA, show = FALSE),
    "patch_size must be a single numeric"
  )
})

test_that("pixel_morph_animate validates downscale_steps", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  expect_error(
    pixel_morph_animate(imgA, imgB, downscale_steps = -1, show = FALSE),
    "downscale_steps must be non-negative"
  )
})

test_that("pixel_morph validates inputs same as animate", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  expect_error(
    pixel_morph(imgA, imgB, upscale = NA, show = FALSE),
    "upscale must be a single numeric"
  )

  expect_error(
    pixel_morph(imgA, imgB, alpha = 0, beta = 0, show = FALSE),
    "alpha and beta cannot both be zero"
  )
})

# ------------------------------------------------------------------------------
# morph_pixel.R: upscale warning path (lines 143-146, 534-537)
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate warns on non-positive upscale", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  expect_warning(
    pixel_morph_animate(imgA, imgB, upscale = -1, n_frames = 2, show = FALSE),
    "upscale must be positive"
  )
})

test_that("pixel_morph warns on non-positive upscale", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  expect_warning(
    pixel_morph(imgA, imgB, upscale = 0, n_frames = 2, show = FALSE),
    "upscale must be positive"
  )
})

# ------------------------------------------------------------------------------
# morph_pixel.R: n_frames warning path (lines 152-155, 543-546)
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate warns on n_frames < 2", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  expect_warning(
    pixel_morph_animate(imgA, imgB, n_frames = 1, show = FALSE),
    "n_frames must be at least 2"
  )
})

# ------------------------------------------------------------------------------
# morph_pixel.R: recursive mode (lines 287-299, 677-689)
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate works with recursive mode", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  result <- pixel_morph_animate(
    imgA, imgB,
    mode = "recursive",
    n_frames = 2,
    show = FALSE,
    patch_size = 4
  )

  expect_true(!is.null(result$animation))
  expect_equal(result$mode, "recursive")
})

test_that("pixel_morph works with recursive mode", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  result <- pixel_morph(
    imgA, imgB,
    mode = "recursive",
    n_frames = 2,
    show = FALSE,
    patch_size = 4
  )

  expect_s3_class(result, "magick-image")
})

# ------------------------------------------------------------------------------
# morph_pixel.R: exact mode with patch_size > 1 (lines 269-285, 659-675)
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate works with exact mode and patch_size > 1", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  result <- pixel_morph_animate(
    imgA, imgB,
    mode = "exact",
    patch_size = 4,
    n_frames = 2,
    show = FALSE
  )

  expect_true(!is.null(result$animation))
})

# ------------------------------------------------------------------------------
# morph_pixel.R: downscale path (lines 316-326, 707-718)
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate with downscale_steps > 0", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  expect_warning(
    result <- pixel_morph_animate(
      imgA, imgB,
      downscale_steps = 1,
      n_frames = 2,
      show = FALSE
    ),
    "overlaps and.*holes"
  )

  expect_true(!is.null(result$animation))
})

# ------------------------------------------------------------------------------
# morph_pixel.R: upscaling in rendering (lines 357-372, 749-764)
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate applies integer upscale", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  result <- pixel_morph_animate(
    imgA, imgB,
    upscale = 2,
    n_frames = 2,
    show = FALSE
  )

  expect_true(!is.null(result$animation))
  expect_equal(result$upscale, 2)
})

test_that("pixel_morph_animate applies fractional upscale", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  result <- pixel_morph_animate(
    imgA, imgB,
    upscale = 1.5,
    n_frames = 2,
    show = FALSE
  )

  expect_true(!is.null(result$animation))
  expect_equal(result$upscale, 1.5)
})

test_that("pixel_morph applies upscale", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  result <- pixel_morph(
    imgA, imgB,
    upscale = 2,
    n_frames = 2,
    show = FALSE
  )

  expect_s3_class(result, "magick-image")
})

# ------------------------------------------------------------------------------
# morph_pixel.R: image resize path (lines 192-198, 583-589)
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate resizes mismatched images", {
  skip_on_cran()
  skip_if_not_installed("magick")

  # Create images with different sizes
  imgA <- magick::image_blank(20, 20, color = "red")
  imgB <- magick::image_blank(30, 25, color = "blue")

  result <- pixel_morph_animate(
    imgA, imgB,
    n_frames = 2,
    show = FALSE
  )

  expect_true(!is.null(result$animation))
  expect_equal(result$width, 20)
  expect_equal(result$height, 20)
})

# ------------------------------------------------------------------------------
# morph_pixel.R: webp format (lines 416-417)
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate saves webp format", {
  skip_on_cran()
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  outfile <- tempfile(fileext = ".webp")
  on.exit(unlink(outfile), add = TRUE)

  result <- pixel_morph_animate(
    imgA, imgB,
    format = "webp",
    outfile = outfile,
    n_frames = 2,
    show = FALSE
  )

  expect_true(file.exists(outfile))
})

# ------------------------------------------------------------------------------
# morph_pixel.R: n_frames = 1 delay path (lines 377-378)
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate handles n_frames = 1 after warning", {
  skip_on_cran()

  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  # This should warn about n_frames < 2 then set to 2
  expect_warning(
    result <- pixel_morph_animate(imgA, imgB, n_frames = 1, show = FALSE),
    "n_frames must be at least 2"
  )
  expect_true(!is.null(result$animation))
})

# ------------------------------------------------------------------------------
# morph_utils.R: %||% operator (line 512)
# ------------------------------------------------------------------------------

test_that("%||% operator works", {
  skip_on_cran()
  `%||%` <- couplr:::`%||%`

  expect_equal(NULL %||% 5, 5)
  expect_equal(3 %||% 5, 3)
  expect_equal("a" %||% "b", "a")
})

# ------------------------------------------------------------------------------
# morph_tiling.R: .generate_square_tiles edge cases
# ------------------------------------------------------------------------------

test_that(".generate_square_tiles handles non-divisible dimensions", {
  skip_on_cran()
  # 7x5 with P=3: core is 6x3, with remainders
  tiles <- couplr:::.generate_square_tiles(W = 7, H = 5, P = 3)

  # Should cover all pixels
  covered <- matrix(FALSE, nrow = 5, ncol = 7)
  for (tile in tiles) {
    for (dx in 0:(tile$size - 1)) {
      for (dy in 0:(tile$size - 1)) {
        covered[tile$y0 + dy + 1, tile$x0 + dx + 1] <- TRUE
      }
    }
  }
  expect_true(all(covered))
})

test_that(".generate_square_tiles handles P larger than image", {
  skip_on_cran()
  tiles <- couplr:::.generate_square_tiles(W = 2, H = 2, P = 5)

  # Should still cover all pixels with size-1 tiles
  expect_true(length(tiles) > 0)
})

# ------------------------------------------------------------------------------
# morph_tiling.R: .recursive_tiling_solver edge cases
# ------------------------------------------------------------------------------

test_that(".recursive_tiling_solver handles small images", {
  skip_on_cran()
  H <- 3
  W <- 3
  N <- H * W

  A_planar <- runif(N * 3) * 255
  B_planar <- runif(N * 3) * 255

  result <- couplr:::.recursive_tiling_solver(
    A_planar, B_planar, H, W,
    patch_size = 2,
    alpha = 1, beta = 0.1
  )

  expect_equal(length(result), N)
  expect_true(all(result >= 1 & result <= N))
})

test_that(".recursive_tiling_solver handles odd dimensions", {
  skip_on_cran()
  H <- 5
  W <- 7
  N <- H * W

  A_planar <- runif(N * 3) * 255
  B_planar <- runif(N * 3) * 255

  result <- couplr:::.recursive_tiling_solver(
    A_planar, B_planar, H, W,
    patch_size = 2,
    alpha = 1, beta = 0
  )

  expect_equal(length(result), N)
})

# ------------------------------------------------------------------------------
# morph_tiling.R: .square_tiling_solver edge cases
# ------------------------------------------------------------------------------

test_that(".square_tiling_solver handles uniform color images", {
  skip_on_cran()
  H <- 6
  W <- 6
  N <- H * W

  # Both images same uniform color
  A_planar <- c(rep(128, N), rep(128, N), rep(128, N))
  B_planar <- c(rep(128, N), rep(128, N), rep(128, N))

  result <- couplr:::.square_tiling_solver(
    A_planar, B_planar, H, W,
    max_tile_size = 2,
    alpha = 1, beta = 0.1
  )

  expect_equal(length(result), N)
})

test_that(".square_tiling_solver handles maximize = TRUE", {
  skip_on_cran()
  H <- 4
  W <- 4
  N <- H * W

  A_planar <- runif(N * 3) * 255
  B_planar <- runif(N * 3) * 255

  result <- couplr:::.square_tiling_solver(
    A_planar, B_planar, H, W,
    max_tile_size = 2,
    alpha = 1, beta = 0,
    maximize = TRUE
  )

  expect_equal(length(result), N)
})

# ------------------------------------------------------------------------------
# morph_utils.R: old .solve_color_walk_pipeline in morph_utils.R (lines 381-483)
# This is different from the one in morph_pixel.R
# ------------------------------------------------------------------------------

test_that("morph_utils .solve_color_walk_pipeline handles varied colors", {
  skip_on_cran()
  H <- 6
  W <- 6
  N <- H * W

  # Create varied color patterns
  set.seed(42)
  A_planar <- runif(N * 3) * 255
  B_planar <- runif(N * 3) * 255

  result <- couplr:::.solve_color_walk_pipeline(
    A_planar, B_planar, H, W,
    quantize_bits = 4,
    method = "hungarian",
    maximize = FALSE
  )

  expect_equal(length(result), N)
  expect_true(all(result >= 1 & result <= N))
})
