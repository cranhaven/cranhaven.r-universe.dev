# Tests for cross-classification (interaction) targets, e.g. calibrating the
# pass rate of "Urban males". An interaction target uses a colon-joined
# composite key: variable = "sex:residence", level = "M:Urban". It adds only a
# rate row (no extra margin equality).

skip_if_not_installed("osqp")

test_that("make_rate_targets builds interaction rows from the interactions list", {
  tg <- make_rate_targets(
    interactions = list("sex:residence" = c("M:Urban" = 0.70, "F:Rural" = 0.60))
  )
  expect_true(all(tg$variable == "sex:residence"))
  expect_equal(tg$level, c("M:Urban", "F:Rural"))
  expect_equal(tg$target_rate, c(0.70, 0.60))
  expect_true("priority" %in% names(tg))
})

test_that("make_rate_targets can combine overall, group and interaction targets", {
  tg <- make_rate_targets(
    overall = 0.7,
    groups = list(sex = c(M = 0.71, F = 0.69)),
    interactions = list("sex:residence" = c("M:Urban" = 0.72))
  )
  expect_true(".overall" %in% tg$variable)
  expect_true("sex" %in% tg$variable)
  expect_true("sex:residence" %in% tg$variable)
})

test_that("an exact interaction target is met for the intersection subgroup", {
  d <- example_rate_data(n = 1500, seed = 7)
  tg <- make_rate_targets(
    interactions = list("sex:residence" = c("M:Urban" = 0.72))
  )
  fit <- calibrate_pass_rates(
    d, outcome = "qualified", weight = "initial_weight",
    group_vars = c("sex", "residence"), targets = tg, mode = "exact"
  )
  expect_true(all(fit$target_check$abs_error < 1e-6))
  # Verify the semantics directly from the calibrated weights.
  w <- fit$data$weight_calibrated
  sub <- d$sex == "M" & d$residence == "Urban"
  expect_equal(sum(w[sub] * d$qualified[sub]) / sum(w[sub]), 0.72, tolerance = 1e-5)
})

test_that("interaction targets also work in soft mode", {
  d <- example_rate_data(n = 1500, seed = 7)
  tg <- make_rate_targets(
    interactions = list("sex:residence" = c("M:Urban" = 0.72))
  )
  fit <- calibrate_pass_rates(
    d, outcome = "qualified", weight = "initial_weight",
    group_vars = c("sex", "residence"), targets = tg, mode = "soft"
  )
  expect_s3_class(fit, "pass_rate_calibration")
  expect_true("sex:residence" %in% fit$target_check$variable)
})

test_that("an interaction target referencing a non-grouping variable errors", {
  d <- example_rate_data(n = 400, seed = 7)
  tg <- data.frame(variable = "sex:income", level = "M:high",
                   target_rate = 0.7, stringsAsFactors = FALSE)
  expect_error(
    calibrate_pass_rates(d, "qualified", "initial_weight",
                         group_vars = c("sex", "residence"), targets = tg,
                         mode = "exact"),
    "group_vars"
  )
})

test_that("an interaction target with mismatched component counts errors", {
  d <- example_rate_data(n = 400, seed = 7)
  tg <- data.frame(variable = "sex:residence", level = "M",
                   target_rate = 0.7, stringsAsFactors = FALSE)
  expect_error(
    calibrate_pass_rates(d, "qualified", "initial_weight",
                         group_vars = c("sex", "residence"), targets = tg,
                         mode = "exact"),
    "component"
  )
})
