# ==============================================================================
# Tests for morph tiling functions to increase coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# .generate_square_tiles tests
# ------------------------------------------------------------------------------

test_that(".generate_square_tiles creates tiles for simple case", {
  skip_on_cran()
  tiles <- couplr:::.generate_square_tiles(W = 6, H = 6, P = 3)

  expect_true(length(tiles) > 0)

  # All tiles should have x0, y0, size
  for (tile in tiles) {
    expect_true("x0" %in% names(tile))
    expect_true("y0" %in% names(tile))
    expect_true("size" %in% names(tile))
  }
})

test_that(".generate_square_tiles covers all pixels", {
  skip_on_cran()
  W <- 7
  H <- 5
  P <- 3

  tiles <- couplr:::.generate_square_tiles(W, H, P)

  # Check that all pixels are covered
  covered <- matrix(FALSE, nrow = H, ncol = W)
  for (tile in tiles) {
    for (dx in 0:(tile$size - 1)) {
      for (dy in 0:(tile$size - 1)) {
        x <- tile$x0 + dx + 1L  # Convert to 1-based
        y <- tile$y0 + dy + 1L
        covered[y, x] <- TRUE
      }
    }
  }

  expect_true(all(covered))
})

test_that(".generate_square_tiles handles P larger than image", {
  skip_on_cran()
  tiles <- couplr:::.generate_square_tiles(W = 2, H = 2, P = 5)

  # Should create 1x1 tiles for each pixel
  expect_equal(length(tiles), 4)
  for (tile in tiles) {
    expect_equal(tile$size, 1L)
  }
})

test_that(".generate_square_tiles handles 1x1 image", {
  skip_on_cran()
  tiles <- couplr:::.generate_square_tiles(W = 1, H = 1, P = 3)

  expect_equal(length(tiles), 1)
  expect_equal(tiles[[1]]$size, 1L)
})

test_that(".generate_square_tiles handles non-divisible dimensions", {
  skip_on_cran()
  W <- 10
  H <- 7
  P <- 3

  tiles <- couplr:::.generate_square_tiles(W, H, P)

  # Check coverage
  covered <- matrix(FALSE, nrow = H, ncol = W)
  for (tile in tiles) {
    for (dx in 0:(tile$size - 1)) {
      for (dy in 0:(tile$size - 1)) {
        x <- tile$x0 + dx + 1L
        y <- tile$y0 + dy + 1L
        if (x <= W && y <= H) {
          covered[y, x] <- TRUE
        }
      }
    }
  }

  expect_true(all(covered))
})

test_that(".generate_square_tiles handles wide image", {
  skip_on_cran()
  tiles <- couplr:::.generate_square_tiles(W = 20, H = 3, P = 3)

  expect_true(length(tiles) > 0)

  # Check all pixels covered
  covered <- matrix(FALSE, nrow = 3, ncol = 20)
  for (tile in tiles) {
    for (dx in 0:(tile$size - 1)) {
      for (dy in 0:(tile$size - 1)) {
        covered[tile$y0 + dy + 1L, tile$x0 + dx + 1L] <- TRUE
      }
    }
  }
  expect_true(all(covered))
})

test_that(".generate_square_tiles handles tall image", {
  skip_on_cran()
  tiles <- couplr:::.generate_square_tiles(W = 3, H = 20, P = 3)

  expect_true(length(tiles) > 0)

  # Check all pixels covered
  covered <- matrix(FALSE, nrow = 20, ncol = 3)
  for (tile in tiles) {
    for (dx in 0:(tile$size - 1)) {
      for (dy in 0:(tile$size - 1)) {
        covered[tile$y0 + dy + 1L, tile$x0 + dx + 1L] <- TRUE
      }
    }
  }
  expect_true(all(covered))
})

test_that(".generate_square_tiles handles P = 1", {
  skip_on_cran()
  tiles <- couplr:::.generate_square_tiles(W = 4, H = 4, P = 1)

  # Should create 16 1x1 tiles
  expect_equal(length(tiles), 16)
  for (tile in tiles) {
    expect_equal(tile$size, 1L)
  }
})

test_that(".generate_square_tiles handles P = 2", {
  skip_on_cran()
  tiles <- couplr:::.generate_square_tiles(W = 6, H = 4, P = 2)

  expect_true(length(tiles) > 0)

  # Check sizes are at most 2
  for (tile in tiles) {
    expect_true(tile$size <= 2)
  }
})

# ------------------------------------------------------------------------------
# .solve_tile_lap tests
# ------------------------------------------------------------------------------

test_that(".solve_tile_lap handles 1x1 tile", {
  skip_on_cran()
  H <- 4
  W <- 4
  N <- H * W
  A_planar <- rep(1.0, 3 * N)
  B_planar <- rep(1.0, 3 * N)

  tile <- list(x0 = 0, y0 = 0, size = 1L)

  result <- couplr:::.solve_tile_lap(tile, A_planar, B_planar, H, W)

  expect_equal(length(result), 1)
  expect_equal(result[1], 1L)  # First pixel
})

test_that(".solve_tile_lap handles 2x2 tile", {
  skip_on_cran()
  H <- 4
  W <- 4
  N <- H * W
  A_planar <- rep(0.0, 3 * N)
  B_planar <- rep(0.0, 3 * N)

  tile <- list(x0 = 0, y0 = 0, size = 2L)

  result <- couplr:::.solve_tile_lap(tile, A_planar, B_planar, H, W)

  expect_equal(length(result), 4)  # 2x2 = 4 pixels
})

test_that(".solve_tile_lap handles 3x3 tile", {
  skip_on_cran()
  H <- 6
  W <- 6
  N <- H * W
  # Create uniform colors
  A_planar <- rep(128.0, 3 * N)
  B_planar <- rep(128.0, 3 * N)

  tile <- list(x0 = 0, y0 = 0, size = 3L)

  result <- couplr:::.solve_tile_lap(tile, A_planar, B_planar, H, W)

  expect_equal(length(result), 9)  # 3x3 = 9 pixels
})

test_that(".solve_tile_lap handles beta = 0", {
  skip_on_cran()
  H <- 4
  W <- 4
  N <- H * W
  A_planar <- rep(0.0, 3 * N)
  B_planar <- rep(0.0, 3 * N)

  tile <- list(x0 = 0, y0 = 0, size = 2L)

  result <- couplr:::.solve_tile_lap(tile, A_planar, B_planar, H, W,
                                     alpha = 1, beta = 0)

  expect_equal(length(result), 4)
})

test_that(".solve_tile_lap handles different alpha values", {
  skip_on_cran()
  H <- 4
  W <- 4
  N <- H * W
  A_planar <- rep(100.0, 3 * N)
  B_planar <- rep(150.0, 3 * N)

  tile <- list(x0 = 1, y0 = 1, size = 2L)

  result <- couplr:::.solve_tile_lap(tile, A_planar, B_planar, H, W,
                                     alpha = 2, beta = 0.5)

  expect_equal(length(result), 4)
})

# ------------------------------------------------------------------------------
# .square_tiling_solver tests
# ------------------------------------------------------------------------------

test_that(".square_tiling_solver returns valid assignment", {
  skip_on_cran()
  H <- 4
  W <- 4
  N <- H * W
  A_planar <- rep(0.0, 3 * N)
  B_planar <- rep(0.0, 3 * N)

  result <- couplr:::.square_tiling_solver(A_planar, B_planar, H, W,
                                            max_tile_size = 2L)

  expect_equal(length(result), N)
  expect_true(all(result >= 1))
  expect_true(all(result <= N))
})

test_that(".square_tiling_solver handles tile size 1", {
  skip_on_cran()
  H <- 3
  W <- 3
  N <- H * W
  A_planar <- rep(100.0, 3 * N)
  B_planar <- rep(100.0, 3 * N)

  result <- couplr:::.square_tiling_solver(A_planar, B_planar, H, W,
                                            max_tile_size = 1L)

  # With 1x1 tiles, should get identity assignment
  expect_equal(result, 1:N)
})

test_that(".square_tiling_solver handles non-square image", {
  skip_on_cran()
  H <- 3
  W <- 5
  N <- H * W
  A_planar <- rep(50.0, 3 * N)
  B_planar <- rep(50.0, 3 * N)

  result <- couplr:::.square_tiling_solver(A_planar, B_planar, H, W,
                                            max_tile_size = 2L)

  expect_equal(length(result), N)
  expect_true(all(result >= 1))
  expect_true(all(result <= N))
})

# ------------------------------------------------------------------------------
# Edge cases and integration tests
# ------------------------------------------------------------------------------

test_that("square tiling handles varying color patterns", {
  skip_on_cran()
  H <- 6
  W <- 6
  N <- H * W

  # Create gradient colors
  A_planar <- numeric(3 * N)
  B_planar <- numeric(3 * N)

  for (i in 1:N) {
    A_planar[i] <- (i - 1) / N * 255  # R gradient
    A_planar[i + N] <- 128            # G constant
    A_planar[i + 2*N] <- 255 - (i - 1) / N * 255  # B inverse gradient

    B_planar[i] <- 255 - (i - 1) / N * 255
    B_planar[i + N] <- 128
    B_planar[i + 2*N] <- (i - 1) / N * 255
  }

  result <- couplr:::.square_tiling_solver(A_planar, B_planar, H, W,
                                            max_tile_size = 3L)

  expect_equal(length(result), N)
  expect_true(all(!is.na(result)))
})

test_that("tiling covers boundary pixels correctly", {
  skip_on_cran()
  # Test image where core region doesn't divide evenly
  W <- 11
  H <- 13
  P <- 3

  tiles <- couplr:::.generate_square_tiles(W, H, P)

  # Verify boundary pixels are covered
  covered <- matrix(FALSE, nrow = H, ncol = W)
  for (tile in tiles) {
    for (dx in 0:(tile$size - 1)) {
      for (dy in 0:(tile$size - 1)) {
        covered[tile$y0 + dy + 1L, tile$x0 + dx + 1L] <- TRUE
      }
    }
  }

  # Check corners
  expect_true(covered[1, 1])  # Top-left
  expect_true(covered[1, W])  # Top-right
  expect_true(covered[H, 1])  # Bottom-left
  expect_true(covered[H, W])  # Bottom-right

  # Check full coverage
  expect_true(all(covered))
})

test_that("tile sizes respect maximum", {
  skip_on_cran()
  tiles <- couplr:::.generate_square_tiles(W = 20, H = 20, P = 4)

  for (tile in tiles) {
    expect_true(tile$size <= 4)
    expect_true(tile$size >= 1)
  }
})
