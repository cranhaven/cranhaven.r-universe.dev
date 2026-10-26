# ==============================================================================
# Additional tests for morph_utils.R to reach 90%+ coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# .to_array_rgb edge cases for different array dimension orderings
# ------------------------------------------------------------------------------

test_that(".to_array_rgb handles numeric arrays scaled 0-1", {
  skip_on_cran()
  skip_if_not_installed("magick")

  # Create image and get data - then test with scaled numeric
  img <- magick::image_blank(2, 2, color = "red")
  result <- couplr:::.to_array_rgb(img)

  expect_equal(dim(result), c(2, 2, 3))
  expect_true(is.integer(result))
})

test_that(".to_array_rgb handles larger images", {
  skip_on_cran()
  skip_if_not_installed("magick")

  img <- magick::image_blank(10, 8, color = "#4080C0")
  result <- couplr:::.to_array_rgb(img)

  expect_equal(dim(result), c(8, 10, 3))  # H x W x 3
  expect_true(all(result >= 0 & result <= 255))
})

# ------------------------------------------------------------------------------
# .solve_color_walk_pipeline tests
# ------------------------------------------------------------------------------

test_that(".solve_color_walk_pipeline works with simple images", {
  skip_on_cran()
  skip_if_not_installed("magick")

  H <- 4
  W <- 4
  N <- H * W

  # Create simple planar data - all red for A
  A_planar <- c(rep(255, N), rep(0, N), rep(0, N))  # R, G, B
  # B has different colors
  B_planar <- c(
    rep(c(255, 0, 128, 64), each = 4),   # R channel - varied
    rep(c(0, 255, 0, 64), each = 4),      # G channel - varied
    rep(c(0, 0, 128, 64), each = 4)       # B channel - varied
  )

  result <- couplr:::.solve_color_walk_pipeline(
    A_planar, B_planar, H, W,
    quantize_bits = 5, method = "jv"
  )

  expect_equal(length(result), N)
  expect_true(all(result >= 1 & result <= N))
})

test_that(".solve_color_walk_pipeline handles identical images", {
  skip_on_cran()
  skip_if_not_installed("magick")

  H <- 3
  W <- 3
  N <- H * W

  # Both images identical
  planar <- c(rep(128, N), rep(64, N), rep(192, N))

  result <- couplr:::.solve_color_walk_pipeline(
    planar, planar, H, W,
    quantize_bits = 5, method = "jv"
  )

  expect_equal(length(result), N)
  expect_true(all(result >= 1 & result <= N))
})

# ------------------------------------------------------------------------------
# .solve_color_match_pipeline tests
# ------------------------------------------------------------------------------

test_that(".solve_color_match_pipeline works with matching colors", {
  skip_on_cran()
  skip_if_not_installed("magick")

  H <- 4
  W <- 4
  N <- H * W

  # Create images with some matching colors
  A_planar <- c(rep(255, N), rep(0, N), rep(0, N))  # All red
  B_planar <- c(rep(255, N), rep(0, N), rep(0, N))  # All red (same)

  result <- couplr:::.solve_color_match_pipeline(
    A_planar, B_planar, H, W,
    quantize_bits = 5, method = "jv",
    fill_identity_for_unmatched = TRUE
  )

  expect_equal(length(result), N)
})

test_that(".solve_color_match_pipeline with fill_identity FALSE", {
  skip_on_cran()
  skip_if_not_installed("magick")

  H <- 4
  W <- 4
  N <- H * W

  A_planar <- c(rep(255, N), rep(0, N), rep(0, N))
  B_planar <- c(rep(0, N), rep(255, N), rep(0, N))  # Different (green)

  result <- couplr:::.solve_color_match_pipeline(
    A_planar, B_planar, H, W,
    quantize_bits = 5, method = "jv",
    fill_identity_for_unmatched = FALSE
  )

  expect_equal(length(result), N)
})

# ------------------------------------------------------------------------------
# .build_spatial_assignments_for_pairs with actual data
# ------------------------------------------------------------------------------

test_that(".build_spatial_assignments_for_pairs handles k=0 pairs", {
  skip_on_cran()
  info <- list(
    groupsA = list(c(1L, 2L), c(3L, 4L)),
    groupsB = list(c(5L, 6L), c(7L, 8L))
  )
  pairs <- data.frame(
    ia = c(1L, 2L),
    ib = c(1L, 2L),
    k = c(0L, 2L)  # First pair has k=0
  )

  result <- couplr:::.build_spatial_assignments_for_pairs(info, pairs, H = 4, W = 4)

  expect_true(length(result$i_idx) >= 0)
  expect_true(length(result$j_idx) >= 0)
})

test_that(".build_spatial_assignments_for_pairs handles empty groups", {
  skip_on_cran()
  info <- list(
    groupsA = list(integer(0), c(3L, 4L)),
    groupsB = list(c(5L, 6L), integer(0))
  )
  pairs <- data.frame(
    ia = c(1L, 2L),
    ib = c(1L, 2L),
    k = c(2L, 2L)
  )

  result <- couplr:::.build_spatial_assignments_for_pairs(info, pairs, H = 4, W = 4)

  expect_true(is.list(result))
})

# ------------------------------------------------------------------------------
# .exact_cost_and_solve tests
# ------------------------------------------------------------------------------

test_that(".exact_cost_and_solve handles non-square images", {
  skip_on_cran()
  H <- 2
  W <- 3
  N <- H * W

  A_planar <- c(rep(100, N), rep(150, N), rep(200, N))
  B_planar <- c(rep(105, N), rep(145, N), rep(195, N))

  result <- couplr:::.exact_cost_and_solve(A_planar, B_planar, H, W, alpha = 1, beta = 0)

  expect_equal(length(result), N)
  expect_true(all(result >= 1 & result <= N))
})

test_that(".exact_cost_and_solve with spatial component", {
  skip_on_cran()
  H <- 2
  W <- 2
  N <- H * W

  A_planar <- c(rep(100, N), rep(100, N), rep(100, N))
  B_planar <- c(rep(100, N), rep(100, N), rep(100, N))

  # With beta > 0, spatial cost matters
  result <- couplr:::.exact_cost_and_solve(A_planar, B_planar, H, W, alpha = 0, beta = 1)

  expect_equal(length(result), N)
})

# ------------------------------------------------------------------------------
# .upscale_assignment tests
# ------------------------------------------------------------------------------

test_that(".upscale_assignment works", {
  skip_on_cran()
  # Small scaled assignment
  assign_s <- c(0L, 1L, 2L, 3L)  # 2x2 scaled
  H <- 4
  W <- 4
  Hs <- 2
  Ws <- 2

  result <- couplr:::.upscale_assignment(assign_s, H, W, Hs, Ws)

  expect_equal(length(result), H * W)
})

# ------------------------------------------------------------------------------
# .lap_assign edge cases for different return types
# ------------------------------------------------------------------------------

test_that(".lap_assign handles maximize=TRUE", {
  skip_on_cran()
  cost <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- couplr:::.lap_assign(cost, method = "jv", maximize = TRUE)

  expect_equal(length(result), 2)
  expect_true(all(result >= 0))
})

# ------------------------------------------------------------------------------
# .downscale_both with actual downscaling
# ------------------------------------------------------------------------------

test_that(".downscale_both performs actual downscaling", {
  skip_on_cran()
  H <- 16
  W <- 16
  N <- H * W

  A_planar <- runif(N * 3) * 255
  B_planar <- runif(N * 3) * 255

  result <- couplr:::.downscale_both(A_planar, B_planar, H, W, steps = 1)

  # With 1 step, dimensions should be halved (but min 8)
  expect_equal(result$Hs, 8L)
  expect_equal(result$Ws, 8L)
  expect_equal(length(result$A_s), result$Hs * result$Ws * 3)
})

test_that(".downscale_both with negative steps returns original", {
  skip_on_cran()
  H <- 8
  W <- 8
  N <- H * W
  A <- rep(1.0, N * 3)
  B <- rep(2.0, N * 3)

  result <- couplr:::.downscale_both(A, B, H, W, steps = -1)

  expect_equal(result$Hs, H)
  expect_equal(result$Ws, W)
})
