test_that("toer works", {
  design_sim <- setup_fujikawa(k = 3, p0 = 0.2)
  set.seed(20230319)
  res1 <- toer(design = design_sim, n = 15, lambda = 0.99, design_params =
      list(epsilon = 2, tau = 0, logbase = exp(1)), iter = 5000)
  set.seed(20230319)
  res2 <- get_details(design = design_sim, n = 15, lambda = 0.99,
    epsilon = 2, tau = 0, logbase = exp(1), iter = 5000)

  # Compare results from toer and get_details
  expect_equal(res1, res2$FWER)

  # Compare with results from baskexact
  expect_true(abs(0.03845609 - res1) < 0.01)
})

test_that("ecd works", {
  set.seed(20230512)
  design <- setup_cpp(k = 3, p0 = 0.2)
  res1 <- ecd(design = design, n = 15, p1 = c(0.2, 0.2, 0.5), lambda = 0.95,
    design_params = list(tune_a = 1, tune_b = 1), iter = 5000)
  set.seed(20230512)
  res2 <- get_details(design = design, n = 15, p1 = c(0.2, 0.2, 0.5), lambda = 0.95,
    tune_a = 1, tune_b = 1, iter = 5000)

  # Compare results from ecd and get_details
  expect_equal(res1, res2$ECD)

  # Compare with results from baskexact
  expect_true(abs(2.338286 - res1) < 0.01)
})
