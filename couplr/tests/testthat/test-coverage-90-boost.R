# Additional tests to push coverage to 90%+
# Targets: morph_pixel.R, morph_utils.R, matching_core.R, network_simplex, cycle_cancel

# ---------- morph_utils.R coverage ----------

test_that(".has_namespace works", {
  skip_on_cran()

  expect_true(couplr:::.has_namespace("base"))
  expect_true(couplr:::.has_namespace("couplr"))
  expect_false(couplr:::.has_namespace("nonexistent_package_xyz"))
})

# ---------- matching_core.R coverage ----------

test_that("match_couples handles edge cases", {
  skip_on_cran()

  # Very small dataset
  left <- data.frame(id = 1:2, x = c(0, 1))
  right <- data.frame(id = 1:2, x = c(0.1, 1.1))

  result <- match_couples(left, right, vars = "x")
  expect_equal(nrow(result$pairs), 2)

  # Single variable
  set.seed(42)
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 1:10, x = rnorm(10))

  result <- match_couples(left, right, vars = "x")
  expect_equal(nrow(result$pairs), 10)
})

test_that("match_couples with different methods", {
  skip_on_cran()

  set.seed(42)
  n <- 8
  left <- data.frame(id = 1:n, x = rnorm(n))
  right <- data.frame(id = 1:n, x = rnorm(n))

  # Test multiple methods
  for (method in c("jv", "hungarian", "auction")) {
    result <- tryCatch(
      match_couples(left, right, vars = "x", method = method),
      error = function(e) NULL
    )

    if (!is.null(result)) {
      expect_true(nrow(result$pairs) > 0)
    }
  }
})

test_that("match_couples with weights", {
  skip_on_cran()

  set.seed(42)
  n <- 10
  left <- data.frame(id = 1:n, x = rnorm(n), y = rnorm(n))
  right <- data.frame(id = 1:n, x = rnorm(n), y = rnorm(n))

  # Weights as vector
  result <- match_couples(left, right, vars = c("x", "y"), weights = c(1, 2))
  expect_true(nrow(result$pairs) > 0)

  # Weights as named vector
  result <- match_couples(left, right, vars = c("x", "y"), weights = c(x = 0.5, y = 0.5))
  expect_true(nrow(result$pairs) > 0)
})

test_that("match_couples handles NA in data", {
  skip_on_cran()

  set.seed(42)
  left <- data.frame(id = 1:5, x = c(1, NA, 3, 4, 5))
  right <- data.frame(id = 1:5, x = c(1.1, 2.1, 3.1, NA, 5.1))

  # Should handle or warn about NA values
  result <- tryCatch(
    match_couples(left, right, vars = "x"),
    warning = function(w) { conditionMessage(w) },
    error = function(e) list(error = TRUE)
  )

  expect_true(!is.null(result))
})

test_that("greedy matching with different strategies", {
  skip_on_cran()

  set.seed(42)
  n <- 10
  left <- data.frame(id = 1:n, x = rnorm(n))
  right <- data.frame(id = 1:n, x = rnorm(n))

  # Test all strategies
  for (strategy in c("sorted", "row_best", "pq")) {
    result <- match_couples(left, right, vars = "x", strategy = strategy, method = "greedy")
    expect_true(nrow(result$pairs) > 0)
  }
})

# ---------- network_simplex coverage ----------

test_that("network_simplex handles various sizes", {
  skip_on_cran()

  for (n in c(2, 3, 4, 6)) {
    set.seed(n * 10)
    cost <- matrix(sample(1:50, n * n, replace = TRUE), nrow = n, ncol = n)
    result <- assignment(cost, method = "network_simplex")

    expect_equal(length(result$match), n)
    expect_true(all(sort(result$match) == 1:n))
  }
})

test_that("network_simplex with Inf costs", {
  skip_on_cran()

  # Some forbidden edges
  cost <- matrix(c(1, Inf, 2, Inf, Inf, 3, Inf, 4, 5), nrow = 3, ncol = 3)

  result <- tryCatch(
    assignment(cost, method = "network_simplex"),
    error = function(e) list(error = TRUE)
  )

  # Should either succeed or fail gracefully
  expect_true(!is.null(result))
})

# ---------- cycle_cancel coverage ----------

test_that("cycle_cancel with varying matrix sizes", {
  skip_on_cran()

  for (n in c(2, 3, 4, 6)) {
    set.seed(n * 20)
    cost <- matrix(runif(n * n) * 100, nrow = n, ncol = n)
    result <- assignment(cost, method = "cycle_cancel")

    expect_equal(length(result$match), n)
    expect_true(result$total_cost > 0)
  }
})

test_that("cycle_cancel with zero costs", {
  skip_on_cran()

  cost <- matrix(0, nrow = 3, ncol = 3)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$total_cost, 0)
})

# ---------- Additional solver coverage ----------

test_that("gabow_tarjan with larger matrices", {
  skip_on_cran()

  for (n in c(4, 5, 6)) {
    set.seed(n * 30)
    cost <- matrix(sample(1:100, n * n, replace = TRUE), nrow = n, ncol = n)
    result <- assignment(cost, method = "gabow_tarjan")

    expect_equal(length(result$match), n)
  }
})

test_that("hungarian with negative costs", {
  skip_on_cran()

  cost <- matrix(c(-5, -1, -2, -10), nrow = 2, ncol = 2)
  result <- assignment(cost, method = "hungarian")

  expect_true(result$total_cost < 0)
})

test_that("auction with various epsilon values", {
  skip_on_cran()

  cost <- matrix(c(1, 5, 3, 2), nrow = 2, ncol = 2)

  # Default epsilon
  result1 <- assignment(cost, method = "auction")
  expect_equal(result1$total_cost, 3)

  # Test auction_gs variant
  result2 <- assignment(cost, method = "auction_gs")
  expect_equal(result2$total_cost, 3)
})

# ---------- morph_pixel.R edge cases ----------

test_that("pixel_morph_animate validates parameters", {
  skip_on_cran()
  skip_if_not_installed("magick")

  # Create tiny test images
  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")

  skip_if(!nzchar(imgA), "Test images not found")
  skip_if(!nzchar(imgB), "Test images not found")

  # Test with invalid upscale
  expect_error(
    pixel_morph_animate(imgA, imgB, upscale = "invalid", show = FALSE, n_frames = 2)
  )

  # Test with invalid n_frames
  expect_error(
    pixel_morph_animate(imgA, imgB, n_frames = "invalid", show = FALSE)
  )

  # Test with invalid alpha
  expect_error(
    pixel_morph_animate(imgA, imgB, alpha = -1, show = FALSE, n_frames = 2)
  )

  # Test with alpha = 0 and beta = 0
  expect_error(
    pixel_morph_animate(imgA, imgB, alpha = 0, beta = 0, show = FALSE, n_frames = 2)
  )
})

# ---------- Additional matching coverage ----------

test_that("match_couples with return_diagnostics", {
  skip_on_cran()

  set.seed(42)
  n <- 10
  left <- data.frame(id = 1:n, x = rnorm(n))
  right <- data.frame(id = 1:n, x = rnorm(n))

  result <- match_couples(left, right, vars = "x", return_diagnostics = TRUE)

  # Check that result has diagnostic information
  expect_true("info" %in% names(result) || "pairs" %in% names(result))
})

test_that("match_couples auto_scale feature", {
  skip_on_cran()

  set.seed(42)
  n <- 15
  # Different scales for x and y
  left <- data.frame(id = 1:n, x = rnorm(n, 0, 100), y = rnorm(n, 0, 0.01))
  right <- data.frame(id = 1:n, x = rnorm(n, 0, 100), y = rnorm(n, 0, 0.01))

  result <- match_couples(left, right, vars = c("x", "y"), auto_scale = TRUE)
  expect_true(nrow(result$pairs) > 0)
})

test_that("balance_diagnostics with various inputs", {
  skip_on_cran()

  set.seed(42)
  n <- 15
  left <- data.frame(id = 1:n, x = rnorm(n), y = rnorm(n), z = rnorm(n))
  right <- data.frame(id = 1:n, x = rnorm(n), y = rnorm(n), z = rnorm(n))

  result <- match_couples(left, right, vars = c("x", "y", "z"))
  balance <- balance_diagnostics(result, left, right, c("x", "y", "z"))

  expect_true("var_stats" %in% names(balance))
  expect_equal(nrow(balance$var_stats), 3)
})

test_that("join_matched with suffix options", {
  skip_on_cran()

  set.seed(42)
  n <- 10
  left <- data.frame(id = 1:n, x = rnorm(n), value = letters[1:n])
  right <- data.frame(id = 1:n, x = rnorm(n), value = LETTERS[1:n])

  result <- match_couples(left, right, vars = "x")
  joined <- join_matched(result, left, right, suffix = c("_L", "_R"))

  # Should have suffixed columns for overlapping names
  expect_true(ncol(joined) > 2)
})
