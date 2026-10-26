# ==============================================================================
# Tests for morphing tiling utilities (morph_tiling.R)
# ==============================================================================

# ------------------------------------------------------------------------------
# .generate_square_tiles tests
# ------------------------------------------------------------------------------

test_that(".generate_square_tiles generates tiles for small images", {
  tiles <- couplr:::.generate_square_tiles(W = 6, H = 6, P = 3)

  expect_type(tiles, "list")
  expect_true(length(tiles) > 0)

  # Check tile structure
  tile1 <- tiles[[1]]
  expect_true("x0" %in% names(tile1))
  expect_true("y0" %in% names(tile1))
  expect_true("size" %in% names(tile1))
})

test_that(".generate_square_tiles covers entire image", {
  W <- 10
  H <- 8
  P <- 3

  tiles <- couplr:::.generate_square_tiles(W, H, P)

  # Build coverage matrix
  covered <- matrix(FALSE, nrow = H, ncol = W)
  for (tile in tiles) {
    for (dx in 0:(tile$size - 1)) {
      for (dy in 0:(tile$size - 1)) {
        y <- tile$y0 + dy + 1
        x <- tile$x0 + dx + 1
        if (y <= H && x <= W) {
          covered[y, x] <- TRUE
        }
      }
    }
  }

  # All pixels should be covered
  expect_true(all(covered))
})

test_that(".generate_square_tiles handles non-divisible dimensions", {
  # Image size not divisible by P
  W <- 7
  H <- 5
  P <- 3

  tiles <- couplr:::.generate_square_tiles(W, H, P)

  # Build coverage matrix
  covered <- matrix(FALSE, nrow = H, ncol = W)
  for (tile in tiles) {
    for (dx in 0:(tile$size - 1)) {
      for (dy in 0:(tile$size - 1)) {
        y <- tile$y0 + dy + 1
        x <- tile$x0 + dx + 1
        if (y <= H && x <= W) {
          covered[y, x] <- TRUE
        }
      }
    }
  }

  expect_true(all(covered))
})

test_that(".generate_square_tiles handles P=1", {
  W <- 4
  H <- 4
  P <- 1

  tiles <- couplr:::.generate_square_tiles(W, H, P)

  # Should have 16 tiles, each of size 1
  expect_equal(length(tiles), 16)
  expect_true(all(sapply(tiles, function(t) t$size) == 1))
})

test_that(".generate_square_tiles handles P larger than image", {
  W <- 2
  H <- 2
  P <- 5

  tiles <- couplr:::.generate_square_tiles(W, H, P)

  # Build coverage matrix
  covered <- matrix(FALSE, nrow = H, ncol = W)
  for (tile in tiles) {
    for (dx in 0:(tile$size - 1)) {
      for (dy in 0:(tile$size - 1)) {
        y <- tile$y0 + dy + 1
        x <- tile$x0 + dx + 1
        if (y <= H && x <= W) {
          covered[y, x] <- TRUE
        }
      }
    }
  }

  expect_true(all(covered))
})

test_that(".generate_square_tiles handles square image", {
  W <- 9
  H <- 9
  P <- 3

  tiles <- couplr:::.generate_square_tiles(W, H, P)

  # Should have 9 tiles of size 3 for a 9x9 image
  sizes <- sapply(tiles, function(t) t$size)
  expect_true(all(sizes == 3))
  expect_equal(length(tiles), 9)
})

test_that(".generate_square_tiles no overlapping tiles", {
  W <- 10
  H <- 8
  P <- 3

  tiles <- couplr:::.generate_square_tiles(W, H, P)

  # Build count matrix
  count <- matrix(0L, nrow = H, ncol = W)
  for (tile in tiles) {
    for (dx in 0:(tile$size - 1)) {
      for (dy in 0:(tile$size - 1)) {
        y <- tile$y0 + dy + 1
        x <- tile$x0 + dx + 1
        if (y <= H && x <= W) {
          count[y, x] <- count[y, x] + 1L
        }
      }
    }
  }

  # Each pixel should be covered exactly once
  expect_true(all(count == 1))
})

# ------------------------------------------------------------------------------
# Edge cases
# ------------------------------------------------------------------------------

test_that(".generate_square_tiles handles 1x1 image", {
  tiles <- couplr:::.generate_square_tiles(W = 1, H = 1, P = 3)

  expect_equal(length(tiles), 1)
  expect_equal(tiles[[1]]$size, 1)
  expect_equal(tiles[[1]]$x0, 0)
  expect_equal(tiles[[1]]$y0, 0)
})

test_that(".generate_square_tiles handles 1xN image", {
  tiles <- couplr:::.generate_square_tiles(W = 5, H = 1, P = 3)

  # Should cover all 5 pixels
  covered <- rep(FALSE, 5)
  for (tile in tiles) {
    for (dx in 0:(tile$size - 1)) {
      x <- tile$x0 + dx + 1
      if (x <= 5) {
        covered[x] <- TRUE
      }
    }
  }

  expect_true(all(covered))
})

test_that(".generate_square_tiles handles Nx1 image", {
  tiles <- couplr:::.generate_square_tiles(W = 1, H = 5, P = 3)

  # Should cover all 5 pixels
  covered <- rep(FALSE, 5)
  for (tile in tiles) {
    for (dy in 0:(tile$size - 1)) {
      y <- tile$y0 + dy + 1
      if (y <= 5) {
        covered[y] <- TRUE
      }
    }
  }

  expect_true(all(covered))
})

test_that(".generate_square_tiles handles large images efficiently", {
  skip_on_cran()

  # Should complete quickly for larger images
  start_time <- Sys.time()
  tiles <- couplr:::.generate_square_tiles(W = 100, H = 100, P = 5)
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  expect_true(elapsed < 5)  # Should be fast
  expect_true(length(tiles) > 0)
})

# ------------------------------------------------------------------------------
# Integration with morphing (if morph functions available)
# ------------------------------------------------------------------------------

test_that("tiling integrates with morph pixel functions", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  skip_if_not(exists("morph_pixel_level_cpp"))

  # Just test that tiling functions are accessible
  tiles <- couplr:::.generate_square_tiles(W = 8, H = 8, P = 3)
  expect_true(length(tiles) > 0)
})
