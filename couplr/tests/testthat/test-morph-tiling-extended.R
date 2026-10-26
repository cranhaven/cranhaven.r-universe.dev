# ==============================================================================
# Extended tests for morph_tiling.R
# ==============================================================================

# ------------------------------------------------------------------------------
# .generate_square_tiles tests
# ------------------------------------------------------------------------------

test_that(".generate_square_tiles handles basic case", {
  skip_on_cran()
  tiles <- couplr:::.generate_square_tiles(6, 6, P = 3)

  # Should have 4 tiles (2x2 grid of 3x3 tiles)
  expect_equal(length(tiles), 4)

  # All tiles should be 3x3
  for (tile in tiles) {
    expect_equal(tile$size, 3)
  }
})

test_that(".generate_square_tiles handles P larger than image", {
  skip_on_cran()
  # P > min(W, H) should result in 1x1 tiles
  tiles <- couplr:::.generate_square_tiles(3, 3, P = 5)

  # Should have 9 tiles (3x3 pixels, each as a separate tile)
  expect_equal(length(tiles), 9)

  # All tiles should be 1x1
  for (tile in tiles) {
    expect_equal(tile$size, 1)
  }
})

test_that(".generate_square_tiles handles non-square image", {
  skip_on_cran()
  tiles <- couplr:::.generate_square_tiles(8, 6, P = 3)

  # All tiles should be <= 3
  for (tile in tiles) {
    expect_lte(tile$size, 3)
  }

  # Total pixels covered should equal image size
  total_pixels <- sum(sapply(tiles, function(t) t$size^2))
  expect_equal(total_pixels, 8 * 6)
})

test_that(".generate_square_tiles handles remainder strips", {
  skip_on_cran()
  # 7x7 with P=3: 2x2 core of 3x3 tiles + remainder strips
  tiles <- couplr:::.generate_square_tiles(7, 7, P = 3)

  # Check total coverage
  total_pixels <- sum(sapply(tiles, function(t) t$size^2))
  expect_equal(total_pixels, 7 * 7)
})

# ------------------------------------------------------------------------------
# .solve_tile_lap tests
# ------------------------------------------------------------------------------

test_that(".solve_tile_lap solves basic tile", {
  skip_on_cran()
  # Create simple 4x4 planar images
  A_planar <- matrix(runif(16 * 3, 0, 255), nrow = 16, ncol = 3)
  B_planar <- matrix(runif(16 * 3, 0, 255), nrow = 16, ncol = 3)

  tile <- list(x0 = 0, y0 = 0, size = 2)
  result <- couplr:::.solve_tile_lap(tile, A_planar, B_planar, H = 4, W = 4,
                                     method = "jv")

  # Returns vector of B indices
  expect_true(is.vector(result))
  expect_length(result, 4)  # 2x2 tile = 4 pixels
})

# ------------------------------------------------------------------------------
# .square_tiling_solver tests
# ------------------------------------------------------------------------------

test_that(".square_tiling_solver runs without error", {
  skip_on_cran()
  # Create simple 6x6 planar images
  A_planar <- matrix(runif(36 * 3, 0, 255), nrow = 36, ncol = 3)
  B_planar <- matrix(runif(36 * 3, 0, 255), nrow = 36, ncol = 3)

  result <- couplr:::.square_tiling_solver(A_planar, B_planar, H = 6, W = 6,
                                           max_tile_size = 3, method = "jv")

  # Should return assignment of length 36
  expect_length(result, 36)
})

# ------------------------------------------------------------------------------
# Morph utils tests
# ------------------------------------------------------------------------------

test_that(".assemble_assignment handles basic case", {
  skip_on_cran()
  i_idx <- c(1, 2, 3)
  j_idx <- c(2, 3, 1)

  assign <- couplr:::.assemble_assignment(N = 3, i_idx, j_idx)

  expect_equal(assign[1], 2)
  expect_equal(assign[2], 3)
  expect_equal(assign[3], 1)
})

test_that(".assemble_assignment handles partial assignment", {
  skip_on_cran()
  i_idx <- c(1, 3)
  j_idx <- c(2, 4)

  assign <- couplr:::.assemble_assignment(N = 5, i_idx, j_idx)

  expect_equal(assign[1], 2)
  expect_equal(assign[3], 4)
})

test_that(".lap_assign solves assignment problem", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- couplr:::.lap_assign(cost, method = "jv")

  expect_length(result, 2)
  # Should find optimal: 1->1, 2->2 or 1->2, 2->1
  expect_true(all(result %in% 0:1))  # 0-based
})
