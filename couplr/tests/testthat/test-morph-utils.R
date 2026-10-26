# ==============================================================================
# Tests for morph utility functions (morph_utils.R)
# ==============================================================================

# ------------------------------------------------------------------------------
# .has_namespace tests
# ------------------------------------------------------------------------------

test_that(".has_namespace returns TRUE for installed packages", {
  expect_true(couplr:::.has_namespace("stats"))
  expect_true(couplr:::.has_namespace("utils"))
})

test_that(".has_namespace returns FALSE for non-existent packages", {
  expect_false(couplr:::.has_namespace("nonexistent_package_xyz123"))
})

test_that(".to_planar_rgb converts array to planar format", {
  # Create a simple 2x3 RGB array
  H <- 2
  W <- 3
  arr <- array(0L, dim = c(H, W, 3))
  arr[,,1] <- matrix(c(1, 2, 3, 4, 5, 6), nrow = H, ncol = W)  # R
  arr[,,2] <- matrix(c(10, 20, 30, 40, 50, 60), nrow = H, ncol = W)  # G
  arr[,,3] <- matrix(c(100, 200, 101, 201, 102, 202), nrow = H, ncol = W)  # B

  planar <- couplr:::.to_planar_rgb(arr)

  expect_equal(length(planar), H * W * 3)
  # First H*W elements are R channel
  expect_equal(planar[1:(H*W)], as.vector(arr[,,1]))
  # Next H*W elements are G channel
  expect_equal(planar[(H*W + 1):(2*H*W)], as.vector(arr[,,2]))
  # Last H*W elements are B channel
  expect_equal(planar[(2*H*W + 1):(3*H*W)], as.vector(arr[,,3]))
})

# ------------------------------------------------------------------------------
# .call_or tests
# ------------------------------------------------------------------------------

test_that(".call_or calls primary function if it exists", {
  # stats::mean exists
  result <- couplr:::.call_or("mean", "nonexistent", 1:10)
  expect_equal(result, 5.5)
})

test_that(".call_or calls fallback if primary doesn't exist", {
  # First arg doesn't exist, but sum does
  result <- couplr:::.call_or("nonexistent_func_xyz", "sum", 1:10)
  expect_equal(result, 55)
})

test_that(".call_or errors if neither exists", {
  expect_error(
    couplr:::.call_or("nonexistent1_xyz", "nonexistent2_xyz"),
    "Neither"
  )
})

# ------------------------------------------------------------------------------
# .downscale_both tests
# ------------------------------------------------------------------------------

test_that(".downscale_both returns originals when steps is 0", {
  H <- 4
  W <- 4
  N <- H * W
  A <- rep(1.0, 3 * N)
  B <- rep(2.0, 3 * N)

  result <- couplr:::.downscale_both(A, B, H, W, steps = 0)

  expect_equal(result$Hs, H)
  expect_equal(result$Ws, W)
  expect_equal(result$A_s, A)
  expect_equal(result$B_s, B)
})

test_that(".downscale_both returns originals when steps is NULL", {
  H <- 4
  W <- 4
  N <- H * W
  A <- rep(1.0, 3 * N)
  B <- rep(2.0, 3 * N)

  result <- couplr:::.downscale_both(A, B, H, W, steps = NULL)

  expect_equal(result$Hs, H)
  expect_equal(result$Ws, W)
})

# ------------------------------------------------------------------------------
# .lap_assign tests
# ------------------------------------------------------------------------------

test_that(".lap_assign returns 0-based assignment", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- couplr:::.lap_assign(cost, method = "hungarian")

  # Should return 0-based indices
  expect_true(all(result >= 0))
  expect_true(all(result <= 1))
  expect_equal(length(result), 2)
})

test_that(".lap_assign handles tibble result from lap_solve", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- couplr:::.lap_assign(cost, method = "jv")

  expect_equal(length(result), 2)
  expect_true(all(result %in% c(0, 1)))
})

# ------------------------------------------------------------------------------
# .fill_unassigned_identity tests
# ------------------------------------------------------------------------------

test_that(".fill_unassigned_identity fills negative values", {
  assign <- c(2L, -1L, 1L, -1L)

  result <- couplr:::.fill_unassigned_identity(assign)

  expect_equal(result[2], 2L)  # Filled with own index
  expect_equal(result[4], 4L)  # Filled with own index
  expect_equal(result[1], 2L)  # Unchanged
  expect_equal(result[3], 1L)  # Unchanged
})

test_that(".fill_unassigned_identity handles all assigned", {
  assign <- c(2L, 1L, 4L, 3L)

  result <- couplr:::.fill_unassigned_identity(assign)

  expect_equal(result, assign)
})

# ------------------------------------------------------------------------------
# .assemble_assignment tests
# ------------------------------------------------------------------------------

test_that(".assemble_assignment builds assignment vector", {
  N <- 4
  i_idx <- c(1L, 3L)
  j_idx <- c(2L, 4L)

  result <- couplr:::.assemble_assignment(N, i_idx, j_idx)

  expect_equal(result[1], 2L)
  expect_equal(result[3], 4L)
  expect_equal(result[2], -1L)  # Unassigned
  expect_equal(result[4], -1L)  # Unassigned
})

test_that(".assemble_assignment handles empty input", {
  N <- 4
  result <- couplr:::.assemble_assignment(N, integer(0), integer(0))

  expect_equal(result, rep(-1L, N))
})

# ------------------------------------------------------------------------------
# %||% operator tests
# ------------------------------------------------------------------------------

test_that("%||% returns first argument if not NULL", {
  expect_equal(couplr:::`%||%`(5, 10), 5)
  expect_equal(couplr:::`%||%`("a", "b"), "a")
})

test_that("%||% returns second argument if first is NULL", {
  expect_equal(couplr:::`%||%`(NULL, 10), 10)
  expect_equal(couplr:::`%||%`(NULL, "default"), "default")
})

# ------------------------------------------------------------------------------
# Image conversion tests (require magick)
# ------------------------------------------------------------------------------

test_that(".to_array_rgb works with magick images", {
  skip_if_not_installed("magick")

  # Create a simple 2x2 red image
  img <- magick::image_blank(2, 2, color = "red")

  result <- couplr:::.to_array_rgb(img)

  expect_equal(dim(result), c(2, 2, 3))
  # Red channel should be 255
  expect_true(all(result[,,1] == 255))
  # Green and blue should be 0
  expect_true(all(result[,,2] == 0))
  expect_true(all(result[,,3] == 0))
})

test_that(".to_array_rgb handles different color spaces", {
  skip_if_not_installed("magick")

  # Create a gradient image
  img <- magick::image_blank(3, 3, color = "#808080")  # Gray

  result <- couplr:::.to_array_rgb(img)

  expect_equal(dim(result), c(3, 3, 3))
  # All channels should be equal for gray
  expect_equal(result[,,1], result[,,2])
  expect_equal(result[,,2], result[,,3])
})

# ------------------------------------------------------------------------------
# Palette pipeline tests
# ------------------------------------------------------------------------------

test_that(".palette_pairs_identity finds matching colors", {
  # This tests the internal palette matching logic
  # We need to mock the info structure
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0, 0, 255, 0), nrow = 2, ncol = 3, byrow = TRUE),
    colorsB_rgb = matrix(c(255, 0, 0, 0, 0, 255), nrow = 2, ncol = 3, byrow = TRUE),
    countsA = c(10, 5),
    countsB = c(8, 7)
  )

  result <- couplr:::.palette_pairs_identity(info)

  # Only red (255,0,0) matches
  expect_equal(nrow(result), 1)
  expect_equal(result$ia, 1)
  expect_equal(result$ib, 1)
  expect_equal(result$k, 8)  # min(10, 8)
})

test_that(".palette_pairs_identity handles no matches", {
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0), nrow = 1, ncol = 3),
    colorsB_rgb = matrix(c(0, 255, 0), nrow = 1, ncol = 3),
    countsA = c(10),
    countsB = c(10)
  )

  result <- couplr:::.palette_pairs_identity(info)

  expect_equal(nrow(result), 0)
})
