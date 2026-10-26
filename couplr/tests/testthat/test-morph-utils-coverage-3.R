# ==============================================================================
# Additional tests for morph_utils.R coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# Helper function tests
# ------------------------------------------------------------------------------

test_that(".to_planar_rgb converts correctly", {
  skip_on_cran()
  to_planar <- couplr:::.to_planar_rgb

  # Simple 2x2x3 array
  arr <- array(0L, dim = c(2, 2, 3))
  arr[,,1] <- matrix(c(10, 20, 30, 40), 2, 2)  # R channel
  arr[,,2] <- matrix(c(50, 60, 70, 80), 2, 2)  # G channel
  arr[,,3] <- matrix(c(90, 100, 110, 120), 2, 2)  # B channel

  planar <- to_planar(arr)

  expect_equal(length(planar), 2 * 2 * 3)
  # First 4 values should be R channel
  expect_equal(planar[1:4], as.vector(arr[,,1]))
})

test_that(".downscale_both handles no downscaling", {
  skip_on_cran()
  skip_if_not_installed("magick")

  downscale_both <- couplr:::.downscale_both

  # Create small planar data
  A_planar <- runif(3 * 8 * 8)
  B_planar <- runif(3 * 8 * 8)

  # No downscaling (steps = 0)
  result <- downscale_both(A_planar, B_planar, H = 8, W = 8, steps = 0)

  expect_equal(result$Hs, 8L)
  expect_equal(result$Ws, 8L)
  expect_equal(result$A_s, A_planar)
  expect_equal(result$B_s, B_planar)

  # NULL steps
  result2 <- downscale_both(A_planar, B_planar, H = 8, W = 8, steps = NULL)
  expect_equal(result2$Hs, 8L)
})

# ------------------------------------------------------------------------------
# .call_or fallback mechanism
# ------------------------------------------------------------------------------

test_that(".call_or falls back correctly", {
  skip_on_cran()
  call_or <- couplr:::.call_or

  # With a non-existent function, should error
  expect_error(
    call_or("nonexistent_func_xyz", "also_nonexistent_abc"),
    "Neither"
  )
})

# ------------------------------------------------------------------------------
# LAP glue functions
# ------------------------------------------------------------------------------

test_that(".lap_assign handles different output formats", {
  skip_on_cran()
  lap_assign <- couplr:::.lap_assign

  # Simple cost matrix
  C <- matrix(c(1, 10, 10, 1), 2, 2)

  # Should return 0-based indices
  result <- lap_assign(C)
  expect_equal(length(result), 2)
  expect_true(all(result %in% c(0, 1)))
})

# ------------------------------------------------------------------------------
# Palette functions
# ------------------------------------------------------------------------------

test_that(".palette_pairs_identity handles empty match", {
  skip_on_cran()
  palette_pairs_identity <- couplr:::.palette_pairs_identity

  # Create info with no common colors
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0), nrow = 1),
    colorsB_rgb = matrix(c(0, 255, 0), nrow = 1),
    countsA = 10,
    countsB = 10
  )

  result <- palette_pairs_identity(info)

  expect_equal(nrow(result), 0)
})

test_that(".assemble_assignment handles empty indices", {
  skip_on_cran()
  assemble <- couplr:::.assemble_assignment

  # Empty indices
  result <- assemble(N = 10, i_idx = integer(0), j_idx = integer(0))

  expect_equal(length(result), 10)
  expect_true(all(result == -1L))
})

test_that(".fill_unassigned_identity fills correctly", {
  skip_on_cran()
  fill <- couplr:::.fill_unassigned_identity

  assign <- c(2L, -1L, 1L, -1L)
  result <- fill(assign)

  expect_equal(result[1], 2L)  # Already assigned
  expect_equal(result[2], 2L)  # Filled with identity (index 2)
  expect_equal(result[3], 1L)  # Already assigned
  expect_equal(result[4], 4L)  # Filled with identity (index 4)
})

test_that(".build_spatial_assignments_for_pairs handles empty pairs", {
  skip_on_cran()
  build <- couplr:::.build_spatial_assignments_for_pairs

  info <- list(groupsA = list(), groupsB = list())
  pairs <- data.frame(ia = integer(), ib = integer(), k = integer())

  result <- build(info, pairs, H = 10, W = 10)

  expect_equal(length(result$i_idx), 0)
  expect_equal(length(result$j_idx), 0)
})

# ------------------------------------------------------------------------------
# .has_namespace helper
# ------------------------------------------------------------------------------

test_that(".has_namespace checks packages", {
  skip_on_cran()
  has_ns <- couplr:::.has_namespace

  expect_true(has_ns("base"))
  expect_false(has_ns("nonexistent_package_xyz_abc"))
})

# ------------------------------------------------------------------------------
# %||% infix operator
# ------------------------------------------------------------------------------

test_that("%||% works correctly", {
  skip_on_cran()
  `%||%` <- couplr:::`%||%`

  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  expect_equal(5 %||% 10, 5)
})
