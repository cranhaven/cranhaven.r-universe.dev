# ==============================================================================
# Additional tests for morph_utils.R coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# Internal helper tests
# ------------------------------------------------------------------------------

test_that(".has_namespace works", {
  skip_on_cran()
  expect_true(couplr:::.has_namespace("base"))
  expect_false(couplr:::.has_namespace("nonexistent_package_xyz"))
})

# ------------------------------------------------------------------------------
# Array conversion tests
# ------------------------------------------------------------------------------

test_that(".to_array_rgb works with magick images", {
  skip_on_cran()
  skip_if_not_installed("magick")

  img <- magick::image_blank(4, 3, color = "red")
  arr <- couplr:::.to_array_rgb(img)

  expect_equal(dim(arr), c(3, 4, 3))  # H x W x 3
  expect_true(is.integer(arr))
})

# ------------------------------------------------------------------------------
# Downscale/upscale tests
# ------------------------------------------------------------------------------

test_that(".downscale_both with no steps returns original", {
  skip_on_cran()
  H <- 4
  W <- 4
  N <- H * W
  A <- rep(128, N * 3)
  B <- rep(64, N * 3)

  result <- couplr:::.downscale_both(A, B, H, W, steps = 0)

  expect_equal(result$Hs, H)
  expect_equal(result$Ws, W)
  expect_equal(result$A_s, A)
  expect_equal(result$B_s, B)
})

test_that(".downscale_both with NULL steps returns original", {
  skip_on_cran()
  H <- 4
  W <- 4
  N <- H * W
  A <- rep(128, N * 3)
  B <- rep(64, N * 3)

  result <- couplr:::.downscale_both(A, B, H, W, steps = NULL)

  expect_equal(result$Hs, H)
  expect_equal(result$Ws, W)
})

# ------------------------------------------------------------------------------
# LAP assign wrapper tests
# ------------------------------------------------------------------------------

test_that(".lap_assign works with cost matrix", {
  skip_on_cran()
  C <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- couplr:::.lap_assign(C, method = "jv", maximize = FALSE)

  expect_equal(length(result), 2)
  expect_true(all(result >= 0))  # 0-based
})

# ------------------------------------------------------------------------------
# Palette pipeline tests
# ------------------------------------------------------------------------------

test_that(".palette_pairs_identity handles empty matches", {
  skip_on_cran()
  # Create info with no matching colors
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0), nrow = 1, ncol = 3),
    colorsB_rgb = matrix(c(0, 255, 0), nrow = 1, ncol = 3),
    countsA = 10,
    countsB = 10
  )

  result <- couplr:::.palette_pairs_identity(info)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that(".palette_pairs_identity handles matching colors", {
  skip_on_cran()
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0, 0, 255, 0), nrow = 2, ncol = 3, byrow = TRUE),
    colorsB_rgb = matrix(c(255, 0, 0, 0, 0, 255), nrow = 2, ncol = 3, byrow = TRUE),
    countsA = c(10, 5),
    countsB = c(8, 6)
  )

  result <- couplr:::.palette_pairs_identity(info)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)  # Only red matches
})

test_that(".assemble_assignment creates correct assignment", {
  skip_on_cran()
  N <- 5
  i_idx <- c(1, 3, 5)
  j_idx <- c(2, 4, 1)

  result <- couplr:::.assemble_assignment(N, i_idx, j_idx)

  expect_equal(length(result), 5)
  expect_equal(result[1], 2L)
  expect_equal(result[3], 4L)
  expect_equal(result[5], 1L)
  expect_equal(result[2], -1L)  # Unassigned
})

test_that(".fill_unassigned_identity fills gaps", {
  skip_on_cran()
  assign <- c(2L, -1L, 4L, -1L, 1L)

  result <- couplr:::.fill_unassigned_identity(assign)

  expect_equal(result[1], 2L)
  expect_equal(result[2], 2L)  # Filled with identity
  expect_equal(result[4], 4L)  # Filled with identity
})

# ------------------------------------------------------------------------------
# NULL coalesce operator tests
# ------------------------------------------------------------------------------

test_that("%||% operator works", {
  skip_on_cran()
  expect_equal(couplr:::`%||%`(NULL, 5), 5)
  expect_equal(couplr:::`%||%`(3, 5), 3)
  expect_equal(couplr:::`%||%`(0, 5), 0)
})

# ------------------------------------------------------------------------------
# prepare_cost_matrix (from zzz.R)
# ------------------------------------------------------------------------------

test_that("prepare_cost_matrix works with numeric matrix", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- couplr:::prepare_cost_matrix(cost, maximize = FALSE)

  expect_true(is.list(result))
})

test_that("prepare_cost_matrix with maximize", {
  skip_on_cran()
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- couplr:::prepare_cost_matrix(cost, maximize = TRUE)

  expect_true(is.list(result))
})

test_that("prepare_cost_matrix errors on non-numeric", {
  skip_on_cran()
  cost <- matrix(c("a", "b", "c", "d"), 2, 2)

  expect_error(
    couplr:::prepare_cost_matrix(cost),
    "must be a numeric matrix"
  )
})

test_that("prepare_cost_matrix errors on NaN", {
  skip_on_cran()
  cost <- matrix(c(1, NaN, 3, 4), 2, 2)

  expect_error(
    couplr:::prepare_cost_matrix(cost),
    "NaN not allowed"
  )
})
