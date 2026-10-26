# ==============================================================================
# Tests for internal morph_utils.R helpers
# ==============================================================================

# These test internal functions that don't require magick/image files

test_that(".to_planar_rgb converts H x W x 3 array to planar", {
  # Create a 2x3x3 RGB array
  arr <- array(1:18, dim = c(2, 3, 3))

  result <- couplr:::.to_planar_rgb(arr)

  expect_length(result, 18)
  expect_type(result, "double")
})

# ------------------------------------------------------------------------------
# .has_namespace tests
# ------------------------------------------------------------------------------

test_that(".has_namespace returns TRUE for base packages", {
  expect_true(couplr:::.has_namespace("stats"))
  expect_true(couplr:::.has_namespace("base"))
})

test_that(".has_namespace returns FALSE for nonexistent packages", {
  expect_false(couplr:::.has_namespace("nonexistent_pkg_xyz123"))
})

# ------------------------------------------------------------------------------
# .call_or helper tests
# ------------------------------------------------------------------------------

test_that(".call_or calls primary function when available", {
  # sum should exist
  result <- couplr:::.call_or("sum", "nonexistent", 1:5)
  expect_equal(result, 15)
})

test_that(".call_or errors when neither function exists", {
  expect_error(
    couplr:::.call_or("nonexistent1", "nonexistent2", 1),
    "Neither"
  )
})

# ------------------------------------------------------------------------------
# .fill_unassigned_identity tests
# ------------------------------------------------------------------------------

test_that(".fill_unassigned_identity fills negatives with index", {
  assign <- c(2L, -1L, 1L, -1L)

  result <- couplr:::.fill_unassigned_identity(assign)

  expect_equal(result, c(2L, 2L, 1L, 4L))
})

test_that(".fill_unassigned_identity keeps positive values", {
  assign <- c(5L, 4L, 3L, 2L, 1L)

  result <- couplr:::.fill_unassigned_identity(assign)

  expect_equal(result, assign)
})

# ------------------------------------------------------------------------------
# .assemble_assignment tests
# ------------------------------------------------------------------------------

test_that(".assemble_assignment creates assignment vector", {
  N <- 5
  i_idx <- c(1L, 3L, 5L)
  j_idx <- c(2L, 4L, 1L)

  result <- couplr:::.assemble_assignment(N, i_idx, j_idx)

  expect_length(result, N)
  expect_equal(result[1], 2L)
  expect_equal(result[3], 4L)
  expect_equal(result[5], 1L)
  expect_equal(result[2], -1L)  # unfilled
})

test_that(".assemble_assignment handles empty input", {
  N <- 3
  result <- couplr:::.assemble_assignment(N, integer(), integer())

  expect_equal(result, rep(-1L, 3))
})

# ------------------------------------------------------------------------------
# %||% operator tests
# ------------------------------------------------------------------------------

test_that("%||% returns left when not NULL", {
  expect_equal(5 %||% 10, 5)
  expect_equal("a" %||% "b", "a")
})

test_that("%||% returns right when left is NULL", {
  expect_equal(NULL %||% 10, 10)
  expect_equal(NULL %||% "default", "default")
})

# ------------------------------------------------------------------------------
# .palette_pairs_identity tests
# ------------------------------------------------------------------------------

test_that(".palette_pairs_identity matches identical colors", {
  # Create mock info structure
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0, 0, 255, 0), nrow = 2, byrow = TRUE),
    colorsB_rgb = matrix(c(255, 0, 0, 0, 0, 255), nrow = 2, byrow = TRUE),
    countsA = c(10, 5),
    countsB = c(8, 3),
    groupsA = list(1:10, 11:15),
    groupsB = list(1:8, 9:11)
  )

  result <- couplr:::.palette_pairs_identity(info)

  expect_s3_class(result, "data.frame")
  # Should match color (255,0,0) which is index 1 in both
  expect_true(1 %in% result$ia)
})

test_that(".palette_pairs_identity returns empty when no matches", {
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0), nrow = 1),
    colorsB_rgb = matrix(c(0, 255, 0), nrow = 1),
    countsA = 10,
    countsB = 10,
    groupsA = list(1:10),
    groupsB = list(1:10)
  )

  result <- couplr:::.palette_pairs_identity(info)

  expect_equal(nrow(result), 0)
})

# ------------------------------------------------------------------------------
# .downscale_both tests (without C++)
# ------------------------------------------------------------------------------

test_that(".downscale_both returns unchanged when steps is NULL", {
  H <- 10
  W <- 10
  A <- rep(128, H * W * 3)
  B <- rep(64, H * W * 3)

  result <- couplr:::.downscale_both(A, B, H, W, steps = NULL)

  expect_equal(result$Hs, H)
  expect_equal(result$Ws, W)
  expect_equal(result$A_s, A)
  expect_equal(result$B_s, B)
})

test_that(".downscale_both returns unchanged when steps is 0", {
  H <- 10
  W <- 10
  A <- rep(128, H * W * 3)
  B <- rep(64, H * W * 3)

  result <- couplr:::.downscale_both(A, B, H, W, steps = 0)

  expect_equal(result$Hs, H)
  expect_equal(result$Ws, W)
})
