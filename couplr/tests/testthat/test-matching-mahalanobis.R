# ==============================================================================
# Tests for Mahalanobis Distance Fixes
# ==============================================================================

test_that("Mahalanobis distance works with well-conditioned data", {
  set.seed(42)
  left <- data.frame(
    id = 1:10,
    x = rnorm(10, 0, 1),
    y = rnorm(10, 0, 1)
  )
  right <- data.frame(
    id = 11:20,
    x = rnorm(10, 0.5, 1),
    y = rnorm(10, 0.5, 1)
  )

  result <- match_couples(left, right, vars = c("x", "y"),
                          distance = "mahalanobis")
  expect_s3_class(result, "matching_result")
  expect_true(nrow(result$pairs) > 0)
})


test_that("Mahalanobis detects near-singular covariance", {
  # Create data with perfectly collinear variables
  left_mat <- matrix(c(1:10, 2 * (1:10)), ncol = 2)
  right_mat <- matrix(c(11:20, 2 * (11:20)), ncol = 2)

  expect_error(
    compute_distance_matrix(left_mat, right_mat, distance = "mahalanobis"),
    "singular or near-singular"
  )
})


test_that("Mahalanobis accepts custom sigma", {
  set.seed(42)
  left_mat <- matrix(rnorm(20), ncol = 2)
  right_mat <- matrix(rnorm(20), ncol = 2)

  # Identity matrix should give same result as euclidean
  sigma <- diag(2)
  maha_dist <- compute_distance_matrix(left_mat, right_mat,
                                       distance = "mahalanobis",
                                       sigma = sigma)
  eucl_dist <- compute_distance_matrix(left_mat, right_mat,
                                       distance = "euclidean")
  expect_equal(maha_dist, eucl_dist, tolerance = 1e-10)
})


test_that("Mahalanobis sigma validation catches wrong dimensions", {
  left_mat <- matrix(rnorm(20), ncol = 2)
  right_mat <- matrix(rnorm(20), ncol = 2)
  bad_sigma <- diag(3)

  expect_error(
    compute_distance_matrix(left_mat, right_mat,
                            distance = "mahalanobis", sigma = bad_sigma),
    "sigma must be a 2 x 2 matrix"
  )
})


test_that("Mahalanobis vectorized matches pooled within-group reference", {
  set.seed(123)
  n <- 5
  left_mat <- matrix(rnorm(n * 3), ncol = 3)
  right_mat <- matrix(rnorm(n * 3), ncol = 3)

  result <- compute_distance_matrix(left_mat, right_mat,
                                    distance = "mahalanobis")

  # Reference: pooled within-group covariance.
  S_L <- cov(left_mat)
  S_R <- cov(right_mat)
  pooled <- ((n - 1) * S_L + (n - 1) * S_R) / (2 * n - 2)
  inv_cov <- solve(pooled)
  expected <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      diff <- left_mat[i, ] - right_mat[j, ]
      expected[i, j] <- sqrt(as.numeric(t(diff) %*% inv_cov %*% diff))
    }
  }

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("Overall-sample covariance is recoverable by passing sigma", {
  set.seed(7)
  n <- 6
  left_mat  <- matrix(rnorm(n * 2), ncol = 2)
  right_mat <- matrix(rnorm(n * 2, mean = 1), ncol = 2)

  overall_cov <- cov(rbind(left_mat, right_mat))
  result <- compute_distance_matrix(left_mat, right_mat,
                                    distance = "mahalanobis",
                                    sigma = overall_cov)

  inv_cov <- solve(overall_cov)
  expected <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      diff <- left_mat[i, ] - right_mat[j, ]
      expected[i, j] <- sqrt(as.numeric(t(diff) %*% inv_cov %*% diff))
    }
  }
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("Mahalanobis falls back to overall covariance with n=1 in a group", {
  left_mat  <- matrix(rnorm(2), ncol = 2)
  right_mat <- matrix(rnorm(20), ncol = 2)

  res <- compute_distance_matrix(left_mat, right_mat,
                                 distance = "mahalanobis")
  expect_true(all(is.finite(res)))
  expect_equal(dim(res), c(1L, 10L))
})


test_that("sigma parameter propagates through match_couples", {
  set.seed(42)
  left <- data.frame(
    id = 1:8,
    x = rnorm(8),
    y = rnorm(8)
  )
  right <- data.frame(
    id = 9:16,
    x = rnorm(8),
    y = rnorm(8)
  )

  # Custom sigma (diagonal = no cross-correlation)
  sigma <- diag(c(2, 0.5))
  result <- match_couples(left, right, vars = c("x", "y"),
                          distance = "mahalanobis", sigma = sigma)
  expect_s3_class(result, "matching_result")
  expect_true(nrow(result$pairs) > 0)
})


test_that("sigma parameter propagates through greedy matching", {
  set.seed(42)
  left <- data.frame(
    id = 1:8,
    x = rnorm(8),
    y = rnorm(8)
  )
  right <- data.frame(
    id = 9:16,
    x = rnorm(8),
    y = rnorm(8)
  )

  sigma <- diag(2)
  result <- match_couples(left, right, vars = c("x", "y"),
                           distance = "mahalanobis", sigma = sigma, method = "greedy")
  expect_s3_class(result, "matching_result")
})
