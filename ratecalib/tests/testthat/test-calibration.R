test_that("soft calibration returns valid weights and preserves margins", {
  skip_if_not_installed("osqp")
  set.seed(1)
  n <- 1500
  d <- data.frame(
    qualified = rbinom(n, 1, 0.67),
    initial_weight = runif(n, 0.5, 2),
    sex = sample(c("M", "F"), n, TRUE),
    residence = sample(c("U", "R"), n, TRUE),
    education5 = sample(paste0("E", 1:5), n, TRUE),
    age5 = sample(paste0("A", 1:5), n, TRUE)
  )

  targets <- make_rate_targets(
    overall = 0.68,
    groups = list(
      sex = c(M = 0.69, F = 0.67),
      residence = c(U = 0.685, R = 0.675)
    )
  )

  fit <- calibrate_pass_rates(
    d,
    outcome = "qualified",
    weight = "initial_weight",
    group_vars = c("sex", "residence", "education5", "age5"),
    targets = targets,
    lower = 0.25,
    upper = 4,
    mode = "soft",
    new_weight = "final_weight"
  )

  expect_s3_class(fit, "pass_rate_calibration")
  expect_true(all(is.finite(fit$data$final_weight)))
  expect_true(all(fit$data$final_weight > 0))
  expect_lt(max(abs(fit$margin_check$relative_change)), 1e-5)
})

test_that("target helper validates and formats targets", {
  x <- make_rate_targets(
    overall = 0.7,
    groups = list(sex = c(M = 0.72, F = 0.68)),
    group_priority = c(sex = 2)
  )
  expect_equal(nrow(x), 3)
  expect_named(x, c("variable", "level", "target_rate", "priority"))
})
