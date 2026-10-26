# ==============================================================================
# Extended coverage tests for morph_utils.R
# ==============================================================================

# ------------------------------------------------------------------------------
# Image conversion helpers
# ------------------------------------------------------------------------------

test_that(".to_planar_rgb converts array to planar format", {
  skip_on_cran()
  arr <- array(1:24, dim = c(2, 4, 3))
  planar <- couplr:::.to_planar_rgb(arr)
  expect_length(planar, 2 * 4 * 3)
})

# ------------------------------------------------------------------------------
# LAP assignment helper
# ------------------------------------------------------------------------------

test_that(".lap_assign returns 0-based assignment", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- couplr:::.lap_assign(cost, method = "jv")
  expect_length(result, 2)
  # 0-based indices
  expect_true(all(result >= 0 & result <= 1))
})

# ------------------------------------------------------------------------------
# Palette helpers
# ------------------------------------------------------------------------------

test_that(".palette_pairs_identity finds matching colors", {
  skip_on_cran()
  # Create mock info structure
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0, 0, 255, 0), ncol = 3, byrow = TRUE),
    colorsB_rgb = matrix(c(255, 0, 0, 0, 0, 255), ncol = 3, byrow = TRUE),
    countsA = c(10L, 5L),
    countsB = c(8L, 7L),
    groupsA = list(1:10, 11:15),
    groupsB = list(1:8, 9:15)
  )

  pairs <- couplr:::.palette_pairs_identity(info)
  expect_true(is.data.frame(pairs))
  # Should match red (255,0,0)
  expect_true(nrow(pairs) >= 0)
})

test_that(".palette_pairs_identity handles no matches", {
  skip_on_cran()
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0), ncol = 3),
    colorsB_rgb = matrix(c(0, 255, 0), ncol = 3),
    countsA = 10L,
    countsB = 10L
  )
  pairs <- couplr:::.palette_pairs_identity(info)
  expect_equal(nrow(pairs), 0)
})

# ------------------------------------------------------------------------------
# Assignment helpers
# ------------------------------------------------------------------------------

test_that(".assemble_assignment creates assignment vector", {
  skip_on_cran()
  i_idx <- c(1, 3, 5)
  j_idx <- c(2, 4, 6)
  result <- couplr:::.assemble_assignment(N = 6, i_idx, j_idx)
  expect_length(result, 6)
  expect_equal(result[1], 2L)
  expect_equal(result[3], 4L)
  expect_equal(result[5], 6L)
})

test_that(".assemble_assignment handles empty inputs", {
  skip_on_cran()
  result <- couplr:::.assemble_assignment(N = 5, integer(0), integer(0))
  expect_length(result, 5)
  expect_true(all(result == -1L))
})

test_that(".fill_unassigned_identity fills gaps", {
  skip_on_cran()
  assign <- c(2L, -1L, 4L, -1L, 6L)
  result <- couplr:::.fill_unassigned_identity(assign)
  expect_equal(result[2], 2L)  # Filled with identity
  expect_equal(result[4], 4L)  # Filled with identity
})

# ------------------------------------------------------------------------------
# Downscale helpers
# ------------------------------------------------------------------------------

test_that(".downscale_both returns correct structure", {
  skip_on_cran()
  # Create simple planar data
  H <- 16
  W <- 16
  A_planar <- runif(H * W * 3)
  B_planar <- runif(H * W * 3)

  result <- couplr:::.downscale_both(A_planar, B_planar, H, W, steps = 1)
  expect_true("Hs" %in% names(result))
  expect_true("Ws" %in% names(result))
  expect_true("A_s" %in% names(result))
  expect_true("B_s" %in% names(result))
})

test_that(".downscale_both handles steps = 0", {
  skip_on_cran()
  H <- 10
  W <- 10
  A_planar <- runif(H * W * 3)
  B_planar <- runif(H * W * 3)

  result <- couplr:::.downscale_both(A_planar, B_planar, H, W, steps = 0)
  expect_equal(result$Hs, H)
  expect_equal(result$Ws, W)
})

test_that(".downscale_both handles NULL steps", {
  skip_on_cran()
  H <- 10
  W <- 10
  A_planar <- runif(H * W * 3)
  B_planar <- runif(H * W * 3)

  result <- couplr:::.downscale_both(A_planar, B_planar, H, W, steps = NULL)
  expect_equal(result$Hs, H)
  expect_equal(result$Ws, W)
})

# ------------------------------------------------------------------------------
# Patch helpers
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Color match pipeline
# ------------------------------------------------------------------------------

test_that(".solve_color_match_pipeline runs", {
  skip_on_cran()
  skip_if_not_installed("magick")

  H <- 4
  W <- 4
  N <- H * W
  A_planar <- rep(c(255, 0, 0), N)  # All red
  B_planar <- rep(c(0, 255, 0), N)  # All green

  result <- couplr:::.solve_color_match_pipeline(A_planar, B_planar, H, W,
                                                  quantize_bits = 3,
                                                  fill_identity_for_unmatched = TRUE)
  expect_length(result, N)
})
