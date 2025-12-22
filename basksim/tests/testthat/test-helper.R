test_that("check_scenarios works", {
  design <- setup_fujikawa(k = 3, p0 = 0.2)
  scenarios <- get_scenarios(design = design, p1 = 0.5)

  expect_error(check_scenarios(scenarios = scenarios[, -1], design = design))
  expect_error(check_scenarios(scenarios = cbind(scenarios, scenarios[, 1]),
    design = design))
  expect_error(check_scenarios(scenarios = list(scenarios),
    design = design))
  expect_error(check_scenarios(scenarios = scenarios[-1, -4], design = design))
  expect_error(check_scenarios(scenarios = cbind(scenarios, c(0.2, 0.2, 1)),
    design = design))
})

test_that("check_p1 works", {
  design <- setup_mml(k = 3, p0 = 0.2)
  expect_equal(check_p1(design = design, p1 = NULL, data = NULL), c(0.2, 0.2, 0.2))
  expect_error(check_p1(design = design, p1 = c(0.1, 0.3, 0.4), data = NULL))
  expect_error(check_p1(design = design, p1 = c(0.2, 0.4), data = NULL))
})

test_that("check_params works", {
  expect_error(check_params(n = c(10, 20), lambda = 0.99, iter = 100))
  expect_error(check_params(n = -3, lambda = 0.99, iter = 100))
  expect_error(check_params(n = 10, lambda = 1, iter = 100))
  expect_error(check_params(n = 10, lambda = 0.99, iter = 40.5))
})
