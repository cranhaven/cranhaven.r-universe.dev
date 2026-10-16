

test_that("plot_observed_fitted returns a ggplot with correct labels and data", {
  skip_if_not_installed("ggplot2")

  fit <- list(
    draws = list(mu = matrix(c(1, 2,
                               3, 4,
                               5, 6), nrow = 3, byrow = TRUE)),
    outcome_col = "speed",
    outcome_label = "Speed"
  )
  class(fit) <- "traffic_fit"

  data <- data.frame(speed = c(10, 20))

  p <- plot_observed_fitted(fit, data)

  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$x, "Observed Speed")
  expect_identical(p$labels$y, "Predicted Speed")

  expect_equal(p$data$observed, c(10, 20))
  expect_equal(p$data$predicted, colMeans(fit$draws$mu))
})



test_that("plot_observed_fitted validates fit class and data type", {
  skip_if_not_installed("ggplot2")

  fit <- list(
    draws = list(mu = matrix(1, nrow = 2, ncol = 2)),
    outcome_col = "speed",
    outcome_label = "Speed"
  )
  data <- data.frame(speed = c(1, 2))

  expect_error(
    plot_observed_fitted(fit, data),
    "`fit` must be a `traffic_fit`",
    fixed = TRUE
  )

  class(fit) <- "traffic_fit"
  expect_error(
    plot_observed_fitted(fit, as.list(data)),
    "`data` must be a data.frame",
    fixed = TRUE
  )
})


test_that("plot_observed_fitted validates draws and mu structure", {
  skip_if_not_installed("ggplot2")

  data <- data.frame(speed = c(1, 2))

  fit0 <- list(outcome_col = "speed", outcome_label = "Speed")
  class(fit0) <- "traffic_fit"
  expect_error(plot_observed_fitted(fit0, data), "`fit$draws` must be a list", fixed = TRUE)

  fit1 <- list(draws = list(), outcome_col = "speed", outcome_label = "Speed")
  class(fit1) <- "traffic_fit"
  expect_error(plot_observed_fitted(fit1, data), "`fit$draws$mu` is required", fixed = TRUE)

  fit2 <- list(draws = list(mu = "nope"), outcome_col = "speed", outcome_label = "Speed")
  class(fit2) <- "traffic_fit"
  expect_error(plot_observed_fitted(fit2, data), "must be numeric", ignore.case = TRUE)
})


test_that("plot_observed_fitted validates outcome_col and data column presence", {
  skip_if_not_installed("ggplot2")

  base_fit <- list(
    draws = list(mu = matrix(1, nrow = 2, ncol = 2)),
    outcome_label = "Speed"
  )
  class(base_fit) <- "traffic_fit"

  data <- data.frame(speed = c(1, 2))

  base_fit$outcome_col <- NULL
  expect_error(
    plot_observed_fitted(base_fit, data),
    "`fit$outcome_col` must be a non-empty character scalar",
    fixed = TRUE
  )

  base_fit$outcome_col <- "missing"
  expect_error(
    plot_observed_fitted(base_fit, data),
    "Required column `missing` not found in `data`.",
    fixed = TRUE
  )
})


test_that("plot_observed_fitted validates outcome_label", {
  skip_if_not_installed("ggplot2")

  fit <- list(
    draws = list(mu = matrix(1, nrow = 2, ncol = 2)),
    outcome_col = "speed"
  )
  class(fit) <- "traffic_fit"

  data <- data.frame(speed = c(1, 2))

  fit$outcome_label <- NULL
  expect_error(
    plot_observed_fitted(fit, data),
    "`fit$outcome_label` must be a non-empty character scalar",
    fixed = TRUE
  )
})


test_that("plot_observed_fitted errors on length mismatch", {
  skip_if_not_installed("ggplot2")

  fit <- list(
    draws = list(mu = matrix(1, nrow = 5, ncol = 3)),
    outcome_col = "speed",
    outcome_label = "Speed"
  )
  class(fit) <- "traffic_fit"

  data <- data.frame(speed = c(10, 20))

  expect_error(
    plot_observed_fitted(fit, data),
    "Length mismatch:",
    fixed = TRUE
  )
})


test_that("plot_observed_fitted accepts vector mu (single draw)", {
  skip_if_not_installed("ggplot2")

  fit <- list(
    draws = list(mu = c(5, 6, 7)),
    outcome_col = "speed",
    outcome_label = "Speed"
  )
  class(fit) <- "traffic_fit"

  data <- data.frame(speed = c(10, 20, 30))

  p <- plot_observed_fitted(fit, data)
  expect_equal(p$data$predicted, c(5, 6, 7))
})


test_that("plot_observed_fitted tolerates NA/Inf values", {
  skip_if_not_installed("ggplot2")

  fit <- list(
    draws = list(mu = matrix(c(NA_real_, Inf,
                               1, -Inf), nrow = 2, byrow = TRUE)),
    outcome_col = "speed",
    outcome_label = "Speed"
  )
  class(fit) <- "traffic_fit"

  data <- data.frame(speed = c(NA_real_, Inf))

  p <- expect_warning(plot_observed_fitted(fit, data), regexp = NA)
  expect_s3_class(p, "ggplot")
})


test_that("plot_observed_fitted handles n = 1 and constants", {
  skip_if_not_installed("ggplot2")

  fit1 <- list(
    draws = list(mu = matrix(5, nrow = 3, ncol = 1)),
    outcome_col = "speed",
    outcome_label = "Speed"
  )
  class(fit1) <- "traffic_fit"

  p1 <- plot_observed_fitted(fit1, data.frame(speed = 42))
  expect_equal(p1$data$predicted, 5)

  fit2 <- list(
    draws = list(mu = matrix(7, nrow = 10, ncol = 4)),
    outcome_col = "speed",
    outcome_label = "Speed"
  )
  class(fit2) <- "traffic_fit"

  p2 <- plot_observed_fitted(fit2, data.frame(speed = rep(7, 4)))
  expect_equal(p2$data$predicted, rep(7, 4))
})


test_that("plot_observed_fitted handles large inputs", {
  skip_if_not_installed("ggplot2")

  set.seed(1)
  mu <- matrix(rnorm(300 * 2000), nrow = 300)

  fit <- list(
    draws = list(mu = mu),
    outcome_col = "speed",
    outcome_label = "Speed"
  )
  class(fit) <- "traffic_fit"

  data <- data.frame(speed = rnorm(ncol(mu)))

  p <- plot_observed_fitted(fit, data)
  expect_equal(nrow(p$data), ncol(mu))
})






