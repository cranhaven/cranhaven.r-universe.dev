test_that("get_results works for bma", {
  design <- setup_bma(k = 3, p0 = 0.2)
  data <- get_data(k = 3, n = 10, p = c(0.2, 0.5, 0.5), iter = 110)

  res1 <- get_results(design = design, n = 10, p1 = c(0.2, 0.5, 0.5),
    lambda = 0.95, pmp0 = 1, iter = 100)
  res2 <- get_results(design = design, n = 10, lambda = 0.95, pmp0 = 1,
    data = data, iter = 110)

  expect_equal(dim(res1), c(100, 3))
  expect_equal(dim(res2), c(110, 3))
  expect_true(all(res1 %in% c(0, 1)))
  expect_true(all(res2 %in% c(0, 1)))
})

test_that("get_results works for mmlglobal", {
  design <- setup_mmlglobal(k = 3, p0 = 0.2)
  data <- get_data(k = 3, n = 10, p = c(0.2, 0.5, 0.5), iter = 110)

  res1 <- get_results(design = design, n = 10, p1 = c(0.2, 0.5, 0.5),
    lambda = 0.95, iter = 100)
  res2 <- get_results(design = design, n = 10, lambda = 0.95, data = data,
    iter = 110)

  expect_equal(dim(res1), c(100, 3))
  expect_equal(dim(res2), c(110, 3))
  expect_true(all(res1 %in% c(0, 1)))
  expect_true(all(res2 %in% c(0, 1)))
})

test_that("get_results works for bhm", {
  design <- setup_bhm(k = 3, p0 = 0.2, p_target = 0.5)
  data <- bhmbasket::simulateScenarios(
    n_subjects_list = list(rep(10, 3)),
    response_rates_list = list(c(0.2, 0.5, 0.5)),
    n_trials = 15
  )

  res1 <- get_results(design = design, n = 10, p1 = c(0.2, 0.5, 0.5),
    lambda = 0.95, tau_scale = 1, iter = 10)
  res2 <- get_results(design = design, n = 10, lambda = 0.95, tau_scale = 1,
    data = data, iter = 15)

  expect_equal(dim(res1), c(10, 3))
  expect_equal(dim(res2), c(15, 3))
  expect_true(all(res1 %in% c(0, 1)))
  expect_true(all(res2 %in% c(0, 1)))

  data <- get_data(k = 3, n = 10, p = c(0.2, 0.5, 0.5), iter = 10)
  expect_error(get_results(design = design, n = 10, lambda = 0.95,
    tau_scale = 1, data = data))
})

test_that("get_results works for exnex", {
  design <- setup_exnex(k = 3, p0 = 0.2)
  data <- bhmbasket::simulateScenarios(
    n_subjects_list = list(rep(10, 3)),
    response_rates_list = list(c(0.2, 0.5, 0.5)),
    n_trials = 15
  )

  res1 <- get_results(design = design, n = 10, p1 = c(0.2, 0.5, 0.5),
    lambda = 0.95, tau_scale = 1, w = 0.5, iter = 10)
  res2 <- get_results(design = design, n = 10, lambda = 0.95, tau_scale = 1,
    w = 0.5, data = data, iter = 15)

  expect_equal(dim(res1), c(10, 3))
  expect_equal(dim(res2), c(15, 3))
  expect_true(all(res1 %in% c(0, 1)))
  expect_true(all(res2 %in% c(0, 1)))

  data <- get_data(k = 3, n = 10, p = c(0.2, 0.5, 0.5), iter = 10)
  expect_error(get_results(design = design, n = 10, lambda = 0.95,
    tau_scale = 1, w = 0.5, data = data))
})

test_that("get_results works for fujikawa", {
  design <- setup_fujikawa(k = 3, p0 = 0.2)
  data <- get_data(k = 3, n = 10, p = c(0.2, 0.5, 0.5), iter = 110)

  res1 <- get_results(design = design, n = 10, p1 = c(0.2, 0.5, 0.5),
    lambda = 0.95, epsilon = 2, tau = 0, iter = 100)
  res2 <- get_results(design = design, n = 10, lambda = 0.95, epsilon = 2,
    tau = 0, data = data, iter = 110)

  expect_equal(dim(res1), c(100, 3))
  expect_equal(dim(res2), c(110, 3))
  expect_true(all(res1 %in% c(0, 1)))
  expect_true(all(res2 %in% c(0, 1)))
})

test_that("get_results works for jsdglobal", {
  design <- setup_jsdglobal(k = 3, p0 = 0.2)
  data <- get_data(k = 3, n = 10, p = c(0.2, 0.5, 0.5), iter = 110)

  res1 <- get_results(design = design, n = 10, p1 = c(0.2, 0.5, 0.5),
    lambda = 0.95, eps_pair = 2, eps_all = 2, iter = 100)
  res2 <- get_results(design = design, n = 10, lambda = 0.95, eps_pair = 2,
    eps_all = 2, data = data, iter = 110)

  expect_equal(dim(res1), c(100, 3))
  expect_equal(dim(res2), c(110, 3))
  expect_true(all(res1 %in% c(0, 1)))
  expect_true(all(res2 %in% c(0, 1)))
})

test_that("get_results works for cpp", {
  design <- setup_cpp(k = 3, p0 = 0.2)
  data <- get_data(k = 3, n = 10, p = c(0.2, 0.5, 0.5), iter = 110)

  res1 <- get_results(design = design, n = 10, p1 = c(0.2, 0.5, 0.5),
    lambda = 0.95, tune_a = 1, tune_b = 1, iter = 100)
  res2 <- get_results(design = design, n = 10, lambda = 0.95, tune_a = 1,
    tune_b = 1, data = data, iter = 110)

  expect_equal(dim(res1), c(100, 3))
  expect_equal(dim(res2), c(110, 3))
  expect_true(all(res1 %in% c(0, 1)))
  expect_true(all(res2 %in% c(0, 1)))
})

test_that("get_results works for cppglobal", {
  design <- setup_cppglobal(k = 3, p0 = 0.2)
  data <- get_data(k = 3, n = 10, p = c(0.2, 0.5, 0.5), iter = 110)

  res1 <- get_results(design = design, n = 10, p1 = c(0.2, 0.5, 0.5),
    lambda = 0.95, tune_a = 1, tune_b = 1, epsilon = 2, iter = 100)
  res2 <- get_results(design = design, n = 10, lambda = 0.95, tune_a = 1,
    tune_b = 1, epsilon = 2, data = data, iter = 110)

  expect_equal(dim(res1), c(100, 3))
  expect_equal(dim(res2), c(110, 3))
  expect_true(all(res1 %in% c(0, 1)))
  expect_true(all(res2 %in% c(0, 1)))
})

test_that("get_results works for mml", {
  design <- setup_mml(k = 3, p0 = 0.2)
  data <- get_data(k = 3, n = 10, p = c(0.2, 0.5, 0.5), iter = 110)

  res1 <- get_results(design = design, n = 10, p1 = c(0.2, 0.5, 0.5),
    lambda = 0.95, tune_a = 1, tune_b = 1, epsilon = 2, iter = 100)
  res2 <- get_results(design = design, n = 10, lambda = 0.95, tune_a = 1,
    tune_b = 1, epsilon = 2, data = data, iter = 110)

  expect_equal(dim(res1), c(100, 3))
  expect_equal(dim(res2), c(110, 3))
  expect_true(all(res1 %in% c(0, 1)))
  expect_true(all(res2 %in% c(0, 1)))
})
