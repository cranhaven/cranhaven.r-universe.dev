# ==============================================================================
# Additional tests for morph utility functions to increase coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# .to_array_rgb edge cases
# ------------------------------------------------------------------------------

test_that(".to_array_rgb handles different magick color formats", {
  skip_on_cran()
  skip_if_not_installed("magick")

  # Blue image
  img <- magick::image_blank(2, 2, color = "blue")
  result <- couplr:::.to_array_rgb(img)
  expect_equal(dim(result), c(2, 2, 3))
  expect_true(all(result[,,3] == 255))  # Blue channel
  expect_true(all(result[,,1] == 0))    # Red channel

  # White image
  img <- magick::image_blank(2, 2, color = "white")
  result <- couplr:::.to_array_rgb(img)
  expect_true(all(result == 255))

  # Black image
  img <- magick::image_blank(2, 2, color = "black")
  result <- couplr:::.to_array_rgb(img)
  expect_true(all(result == 0))
})

test_that(".to_array_rgb handles hex colors", {
  skip_on_cran()
  skip_if_not_installed("magick")

  img <- magick::image_blank(2, 2, color = "#FF8800")
  result <- couplr:::.to_array_rgb(img)
  expect_equal(dim(result), c(2, 2, 3))
  expect_true(all(result[,,1] == 255))  # Red
  expect_true(all(result[,,2] == 136))  # Green (0x88 = 136)
  expect_true(all(result[,,3] == 0))    # Blue
})

# ------------------------------------------------------------------------------
# .build_spatial_assignments_for_pairs tests
# ------------------------------------------------------------------------------

test_that(".build_spatial_assignments_for_pairs handles empty pairs", {
  skip_on_cran()
  info <- list(groupsA = list(), groupsB = list())
  pairs <- data.frame(ia = integer(), ib = integer(), k = integer())

  result <- couplr:::.build_spatial_assignments_for_pairs(info, pairs, H = 4, W = 4)

  expect_equal(length(result$i_idx), 0)
  expect_equal(length(result$j_idx), 0)
})

# ------------------------------------------------------------------------------
# .lap_assign edge cases
# ------------------------------------------------------------------------------

test_that(".lap_assign handles larger matrices", {
  skip_on_cran()
  cost <- matrix(runif(25), 5, 5)

  result <- couplr:::.lap_assign(cost, method = "hungarian")

  expect_equal(length(result), 5)
  expect_true(all(result >= 0))
  expect_true(all(result <= 4))
  expect_equal(length(unique(result)), 5)  # Bijection
})

test_that(".lap_assign handles rectangular matrices", {
  skip_on_cran()
  cost <- matrix(runif(12), 3, 4)

  result <- couplr:::.lap_assign(cost, method = "jv")

  expect_equal(length(result), 3)
  expect_true(all(result >= 0))
  expect_true(all(result <= 3))
})

# ------------------------------------------------------------------------------
# .exact_cost_and_solve tests
# ------------------------------------------------------------------------------

test_that(".exact_cost_and_solve computes and solves assignment", {
  skip_on_cran()
  H <- 2
  W <- 2
  N <- H * W
  A_planar <- rep(c(255, 0, 0), each = N)  # All red
  B_planar <- c(rep(c(255, 0, 0), each = 1), rep(c(0, 255, 0), each = 1),
                rep(c(0, 0, 255), each = 1), rep(c(128, 128, 128), each = 1))
  # Reshape B_planar to be planar format
  B_planar <- c(255, 0, 0, 128, 0, 255, 0, 128, 0, 0, 255, 128)

  result <- couplr:::.exact_cost_and_solve(A_planar, B_planar, H, W, alpha = 1, beta = 0)

  expect_equal(length(result), N)
  expect_true(all(result >= 1))
  expect_true(all(result <= N))
})

# ------------------------------------------------------------------------------
# .assemble_assignment edge cases
# ------------------------------------------------------------------------------

test_that(".assemble_assignment handles mismatched lengths", {
  skip_on_cran()
  N <- 5
  i_idx <- c(1L, 2L, 3L)
  j_idx <- c(4L, 5L)  # Shorter than i_idx

  result <- couplr:::.assemble_assignment(N, i_idx, j_idx)

  expect_equal(length(result), N)
  # Only first 2 should be assigned
  expect_true(result[1] > 0)
  expect_true(result[2] > 0)
  expect_equal(result[3], -1L)
})

test_that(".assemble_assignment handles large indices", {
  skip_on_cran()
  N <- 10
  i_idx <- c(1L, 5L, 10L)
  j_idx <- c(2L, 6L, 9L)

  result <- couplr:::.assemble_assignment(N, i_idx, j_idx)

  expect_equal(result[1], 2L)
  expect_equal(result[5], 6L)
  expect_equal(result[10], 9L)
})

# ------------------------------------------------------------------------------
# .fill_unassigned_identity edge cases
# ------------------------------------------------------------------------------

test_that(".fill_unassigned_identity handles all unassigned", {
  skip_on_cran()
  assign <- rep(-1L, 5)

  result <- couplr:::.fill_unassigned_identity(assign)

  expect_equal(result, 1:5)
})

test_that(".fill_unassigned_identity handles alternating pattern", {
  skip_on_cran()
  assign <- c(10L, -1L, 10L, -1L, 10L)

  result <- couplr:::.fill_unassigned_identity(assign)

  expect_equal(result[1], 10L)
  expect_equal(result[2], 2L)  # Filled with own index
  expect_equal(result[3], 10L)
  expect_equal(result[4], 4L)  # Filled with own index
  expect_equal(result[5], 10L)
})
