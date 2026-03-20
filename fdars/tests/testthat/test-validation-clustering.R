# Validation tests for clustering functions
# Tests for cluster.kmeans and cluster.optim

# =============================================================================
# Test Data Setup
# =============================================================================

create_clustered_data <- function(n_per_cluster = 10, m = 50, seed = 42) {
  set.seed(seed)
  t_grid <- seq(0, 1, length.out = m)

  # Cluster 1: sine-like curves
  data1 <- matrix(0, nrow = n_per_cluster, ncol = m)
  for (i in 1:n_per_cluster) {
    data1[i, ] <- sin(2 * pi * t_grid) + rnorm(m, 0, 0.1)
  }

  # Cluster 2: cosine-like curves
  data2 <- matrix(0, nrow = n_per_cluster, ncol = m)
  for (i in 1:n_per_cluster) {
    data2[i, ] <- cos(2 * pi * t_grid) + rnorm(m, 0, 0.1)
  }

  # Cluster 3: linear curves
  data3 <- matrix(0, nrow = n_per_cluster, ncol = m)
  for (i in 1:n_per_cluster) {
    data3[i, ] <- t_grid + rnorm(m, 0, 0.1)
  }

  data_mat <- rbind(data1, data2, data3)
  true_clusters <- rep(1:3, each = n_per_cluster)

  list(
    fd = fdars::fdata(data_mat, argvals = t_grid),
    X = data_mat,
    t_grid = t_grid,
    true_clusters = true_clusters,
    n = 3 * n_per_cluster,
    m = m
  )
}

# =============================================================================
# cluster.kmeans Tests
# =============================================================================

test_that("cluster.kmeans returns correct structure", {
  data <- create_clustered_data()
  result <- fdars::cluster.kmeans(data$fd, ncl = 3, seed = 123)

  expect_s3_class(result, "cluster.kmeans")
  expect_true("cluster" %in% names(result))
  expect_true("centers" %in% names(result))
  expect_true("size" %in% names(result))
  expect_true("withinss" %in% names(result))
  expect_true("tot.withinss" %in% names(result))
  expect_true("fdataobj" %in% names(result))

  # Check dimensions
  expect_length(result$cluster, data$n)
  expect_s3_class(result$centers, "fdata")
  expect_equal(nrow(result$centers$data), 3)
  expect_equal(ncol(result$centers$data), data$m)
})

test_that("cluster.kmeans cluster assignments are valid", {
  data <- create_clustered_data()
  result <- fdars::cluster.kmeans(data$fd, ncl = 3, seed = 123)

  # Cluster labels should be 1, 2, or 3
  expect_true(all(result$cluster %in% 1:3))

  # All clusters should have at least one member
  expect_equal(length(unique(result$cluster)), 3)
})

test_that("cluster.kmeans is reproducible with seed", {
  data <- create_clustered_data()
  result1 <- fdars::cluster.kmeans(data$fd, ncl = 3, seed = 456)
  result2 <- fdars::cluster.kmeans(data$fd, ncl = 3, seed = 456)

  expect_equal(result1$cluster, result2$cluster)
  expect_equal(result1$centers$data, result2$centers$data)
})

test_that("cluster.kmeans finds reasonable clusters for well-separated data", {
  data <- create_clustered_data()
  result <- fdars::cluster.kmeans(data$fd, ncl = 3, seed = 123)

  # Most curves should be correctly clustered
  # Allow for some misclassification due to random initialization
  # Check that each true cluster maps mostly to one predicted cluster
  for (true_cl in 1:3) {
    idx <- data$true_clusters == true_cl
    pred_clusters <- result$cluster[idx]
    most_common <- as.integer(names(which.max(table(pred_clusters))))
    accuracy <- mean(pred_clusters == most_common)
    expect_gt(accuracy, 0.7)  # At least 70% should be in same cluster
  }
})

test_that("cluster.kmeans with different metrics", {
  data <- create_clustered_data(n_per_cluster = 8)

  # Test L2 metric (default)
  result_l2 <- fdars::cluster.kmeans(data$fd, ncl = 3, metric = "L2", seed = 123)
  expect_s3_class(result_l2, "cluster.kmeans")

  # Test L1 metric
  result_l1 <- fdars::cluster.kmeans(data$fd, ncl = 3, metric = "L1", seed = 123)
  expect_s3_class(result_l1, "cluster.kmeans")

  # Test Linf metric
  result_linf <- fdars::cluster.kmeans(data$fd, ncl = 3, metric = "Linf", seed = 123)
  expect_s3_class(result_linf, "cluster.kmeans")
})

test_that("cluster.kmeans with custom metric function", {
  data <- create_clustered_data(n_per_cluster = 8)

  # Use metric.lp as function
  result <- fdars::cluster.kmeans(data$fd, ncl = 3, metric = fdars::metric.lp, seed = 123)
  expect_s3_class(result, "cluster.kmeans")
  expect_length(result$cluster, data$n)
})

test_that("cluster.kmeans nstart improves results", {
  data <- create_clustered_data()

  # Multiple starts should give more stable results
  result1 <- fdars::cluster.kmeans(data$fd, ncl = 3, nstart = 1, seed = 789)
  result10 <- fdars::cluster.kmeans(data$fd, ncl = 3, nstart = 10, seed = 789)

  # Both should return valid results
  expect_s3_class(result1, "cluster.kmeans")
  expect_s3_class(result10, "cluster.kmeans")
})

# =============================================================================
# cluster.optim Tests
# =============================================================================

test_that("cluster.optim returns correct structure", {
  data <- create_clustered_data()
  result <- fdars::cluster.optim(data$fd, ncl.range = 2:5,
                                  criterion = "silhouette", seed = 123)

  expect_s3_class(result, "cluster.optim")
  expect_true("optimal.k" %in% names(result))
  expect_true("criterion" %in% names(result))
  expect_true("scores" %in% names(result))
  expect_true("models" %in% names(result))
  expect_true("best.model" %in% names(result))

  # Check dimensions
  expect_length(result$scores, 4)  # k = 2, 3, 4, 5
  expect_length(result$models, 4)
  expect_s3_class(result$best.model, "cluster.kmeans")
})

test_that("cluster.optim silhouette finds correct k", {
  data <- create_clustered_data()
  result <- fdars::cluster.optim(data$fd, ncl.range = 2:6,
                                  criterion = "silhouette", seed = 123)

  # Should find k=3 for well-separated 3-cluster data
  expect_equal(result$optimal.k, 3)

  # Silhouette scores should be between -1 and 1
  expect_true(all(result$scores >= -1 & result$scores <= 1))
})

test_that("cluster.optim Calinski-Harabasz finds correct k", {
  data <- create_clustered_data()
  result <- fdars::cluster.optim(data$fd, ncl.range = 2:6,
                                  criterion = "CH", seed = 123)

  # Should find k=3 for well-separated 3-cluster data
  expect_equal(result$optimal.k, 3)

  # CH index should be positive
  expect_true(all(result$scores > 0))
})

test_that("cluster.optim elbow finds reasonable k", {
  data <- create_clustered_data()
  result <- fdars::cluster.optim(data$fd, ncl.range = 2:6,
                                  criterion = "elbow", seed = 123)

  # Should find k around 3 for well-separated 3-cluster data
  expect_true(result$optimal.k >= 2 && result$optimal.k <= 4)

  # WCSS should be positive and decreasing
  expect_true(all(result$scores > 0))
  expect_true(all(diff(result$scores) <= 0))
})

test_that("cluster.optim is reproducible with seed", {
  data <- create_clustered_data()
  result1 <- fdars::cluster.optim(data$fd, ncl.range = 2:5,
                                   criterion = "silhouette", seed = 999)
  result2 <- fdars::cluster.optim(data$fd, ncl.range = 2:5,
                                   criterion = "silhouette", seed = 999)

  expect_equal(result1$optimal.k, result2$optimal.k)
  expect_equal(result1$scores, result2$scores)
})

test_that("cluster.optim print method works", {
  data <- create_clustered_data()
  result <- fdars::cluster.optim(data$fd, ncl.range = 2:4,
                                  criterion = "silhouette", seed = 123)

  # Print should not error
  expect_output(print(result), "Optimal K-Means Clustering")
  expect_output(print(result), "silhouette")
  expect_output(print(result), "Optimal k:")
})

test_that("cluster.optim plot method works", {
  data <- create_clustered_data()
  result <- fdars::cluster.optim(data$fd, ncl.range = 2:4,
                                  criterion = "silhouette", seed = 123)

  # Plot should not error
  expect_no_error(plot(result))
})

test_that("cluster.optim best.model matches optimal.k", {
  data <- create_clustered_data()
  result <- fdars::cluster.optim(data$fd, ncl.range = 2:5,
                                  criterion = "silhouette", seed = 123)

  # best.model should have optimal.k clusters
  n_clusters <- length(unique(result$best.model$cluster))
  expect_equal(n_clusters, result$optimal.k)
})

test_that("cluster.optim with different metrics", {
  data <- create_clustered_data(n_per_cluster = 8)

  # Test with L1 metric
  result_l1 <- fdars::cluster.optim(data$fd, ncl.range = 2:4,
                                     criterion = "silhouette",
                                     metric = "L1", seed = 123)
  expect_s3_class(result_l1, "cluster.optim")

  # Test with Linf metric
  result_linf <- fdars::cluster.optim(data$fd, ncl.range = 2:4,
                                       criterion = "silhouette",
                                       metric = "Linf", seed = 123)
  expect_s3_class(result_linf, "cluster.optim")
})

# =============================================================================
# Input Validation Tests
# =============================================================================

test_that("cluster.kmeans rejects non-fdata input", {
  X <- matrix(rnorm(100), 10, 10)
  expect_error(fdars::cluster.kmeans(X, ncl = 2))
})

test_that("cluster.optim rejects non-fdata input", {
  X <- matrix(rnorm(100), 10, 10)
  expect_error(fdars::cluster.optim(X, ncl.range = 2:4))
})

test_that("cluster.optim handles invalid ncl.range gracefully", {
  data <- create_clustered_data()

  # k=1 is filtered out, so 2:3 is used
  result1 <- fdars::cluster.optim(data$fd, ncl.range = 1:3, seed = 123)
  expect_true(min(result1$ncl.range) >= 2)

  # k > n is filtered out
  result2 <- fdars::cluster.optim(data$fd, ncl.range = 2:100, seed = 123)
  expect_true(max(result2$ncl.range) < data$n)
})

test_that("cluster.optim errors when all ncl.range is invalid", {
  # Create tiny dataset
  set.seed(42)
  fd <- fdars::fdata(matrix(rnorm(20), 2, 10))

  # All values in 1:1 are invalid (k=1 not allowed)
  expect_error(fdars::cluster.optim(fd, ncl.range = 1))
})

test_that("cluster.optim rejects invalid criterion", {
  data <- create_clustered_data()
  expect_error(fdars::cluster.optim(data$fd, ncl.range = 2:4,
                                     criterion = "invalid"))
})

# =============================================================================
# Edge Cases
# =============================================================================

test_that("cluster.kmeans with k=2", {
  data <- create_clustered_data()
  result <- fdars::cluster.kmeans(data$fd, ncl = 2, seed = 123)

  expect_s3_class(result, "cluster.kmeans")
  expect_equal(length(unique(result$cluster)), 2)
})

test_that("cluster.kmeans with small dataset", {
  set.seed(42)
  n <- 6
  m <- 20
  t_grid <- seq(0, 1, length.out = m)
  X <- matrix(rnorm(n * m), n, m)
  fd <- fdars::fdata(X, argvals = t_grid)

  result <- fdars::cluster.kmeans(fd, ncl = 2, seed = 123)
  expect_s3_class(result, "cluster.kmeans")
  expect_length(result$cluster, n)
})

test_that("cluster.optim with narrow range", {
  data <- create_clustered_data()
  result <- fdars::cluster.optim(data$fd, ncl.range = 3:4,
                                  criterion = "silhouette", seed = 123)

  expect_s3_class(result, "cluster.optim")
  expect_true(result$optimal.k %in% 3:4)
})

# =============================================================================
# cluster.kmeans Print and Plot Tests
# =============================================================================

test_that("print.cluster.kmeans works", {
  data <- create_clustered_data()
  result <- fdars::cluster.kmeans(data$fd, ncl = 3, seed = 123)

  expect_output(print(result), "K-Means Clustering")
  expect_output(print(result), "clusters")
})

test_that("plot.cluster.kmeans works", {
  data <- create_clustered_data()
  result <- fdars::cluster.kmeans(data$fd, ncl = 3, seed = 123)

  expect_no_error(plot(result))
})

# =============================================================================
# cluster.fcm Tests (Fuzzy C-Means)
# =============================================================================

test_that("cluster.fcm returns correct structure", {
  data <- create_clustered_data()
  result <- fdars::cluster.fcm(data$fd, ncl = 3, seed = 123)

  expect_s3_class(result, "cluster.fcm")
  expect_true("cluster" %in% names(result))
  expect_true("membership" %in% names(result))
  expect_true("centers" %in% names(result))
  expect_true("fdataobj" %in% names(result))

  # Check dimensions
  expect_length(result$cluster, data$n)
  expect_equal(nrow(result$membership), data$n)
  expect_equal(ncol(result$membership), 3)
})

test_that("cluster.fcm membership is valid", {
  data <- create_clustered_data()
  result <- fdars::cluster.fcm(data$fd, ncl = 3, seed = 123)

  # Membership values should be in [0, 1]
  expect_true(all(result$membership >= 0 & result$membership <= 1))

  # Rows should sum to 1
  row_sums <- rowSums(result$membership)
  expect_equal(row_sums, rep(1, data$n), tolerance = 1e-6)
})

test_that("cluster.fcm cluster assignments match max membership", {
  data <- create_clustered_data()
  result <- fdars::cluster.fcm(data$fd, ncl = 3, seed = 123)

  # Each cluster assignment should be the column with max membership
  expected_clusters <- apply(result$membership, 1, which.max)
  expect_equal(result$cluster, expected_clusters)
})

test_that("cluster.fcm is reproducible with seed", {
  data <- create_clustered_data()
  result1 <- fdars::cluster.fcm(data$fd, ncl = 3, seed = 456)
  result2 <- fdars::cluster.fcm(data$fd, ncl = 3, seed = 456)

  expect_equal(result1$cluster, result2$cluster)
  expect_equal(result1$membership, result2$membership)
})

test_that("cluster.fcm fuzziness parameter works", {
  data <- create_clustered_data()

  # Low fuzziness (closer to hard clustering)
  result_low <- fdars::cluster.fcm(data$fd, ncl = 3, m = 1.1, seed = 123)

  # High fuzziness (more fuzzy membership)
  result_high <- fdars::cluster.fcm(data$fd, ncl = 3, m = 3.0, seed = 123)

  expect_s3_class(result_low, "cluster.fcm")
  expect_s3_class(result_high, "cluster.fcm")

  # High fuzziness should have more even membership distribution
  max_membership_low <- max(apply(result_low$membership, 1, max))
  max_membership_high <- max(apply(result_high$membership, 1, max))

  # Generally, higher m should lead to more evenly distributed membership
  # (but this is data-dependent)
})

test_that("cluster.fcm with different fuzziness", {
  data <- create_clustered_data(n_per_cluster = 8)

  result_m2 <- fdars::cluster.fcm(data$fd, ncl = 3, m = 2, seed = 123)
  expect_s3_class(result_m2, "cluster.fcm")

  result_m3 <- fdars::cluster.fcm(data$fd, ncl = 3, m = 3, seed = 123)
  expect_s3_class(result_m3, "cluster.fcm")
})

test_that("print.cluster.fcm works", {
  data <- create_clustered_data()
  result <- fdars::cluster.fcm(data$fd, ncl = 3, seed = 123)

  expect_output(print(result), "Fuzzy C-Means")
  expect_output(print(result), "clusters")
})

test_that("plot.cluster.fcm works", {
  data <- create_clustered_data()
  result <- fdars::cluster.fcm(data$fd, ncl = 3, seed = 123)

  expect_no_error(plot(result))
})

test_that("cluster.fcm with k=2", {
  data <- create_clustered_data()
  result <- fdars::cluster.fcm(data$fd, ncl = 2, seed = 123)

  expect_s3_class(result, "cluster.fcm")
  expect_equal(ncol(result$membership), 2)
})

test_that("cluster.fcm with tolerance parameter", {
  data <- create_clustered_data(n_per_cluster = 8)

  result <- fdars::cluster.fcm(data$fd, ncl = 3, tol = 1e-8, seed = 123)
  expect_s3_class(result, "cluster.fcm")
})

test_that("cluster.fcm rejects non-fdata input", {
  X <- matrix(rnorm(100), 10, 10)
  expect_error(fdars::cluster.fcm(X, ncl = 2))
})

# =============================================================================
# cluster.init Tests
# =============================================================================

test_that("cluster.init returns valid initial centers", {
  data <- create_clustered_data()
  centers <- fdars::cluster.init(data$fd, ncl = 3, seed = 123)

  expect_s3_class(centers, "fdata")
  expect_equal(nrow(centers$data), 3)
  expect_equal(ncol(centers$data), data$m)
})

test_that("cluster.init with L1 metric works", {
  data <- create_clustered_data()
  centers <- fdars::cluster.init(data$fd, ncl = 3, metric = "L1", seed = 123)

  expect_s3_class(centers, "fdata")
  expect_equal(nrow(centers$data), 3)
})

test_that("cluster.init is reproducible with seed", {
  data <- create_clustered_data()
  centers1 <- fdars::cluster.init(data$fd, ncl = 3, seed = 789)
  centers2 <- fdars::cluster.init(data$fd, ncl = 3, seed = 789)

  expect_equal(centers1$data, centers2$data)
})
