test_that("save result test", {
  # check the parsing and after saving
  Phi_fn <- function(theta, lambda, alpha) 2 * theta + alpha * lambda
  Psi_fn <- function(theta, lambda, alpha) 2 * lambda + alpha * theta
  res <- semislv(1, 1, Phi_fn, Psi_fn, save = list(time = FALSE), method = "iterative", alpha = 1)
  res <- unclass(res)
  expect_equal("run.time" %in% names(res), FALSE)
  expect_equal(length(res$res_path), 0)
  expect_equal(res$theta, res$iterspace$parameters$theta)
  expect_equal(res$lambda, res$iterspace$parameters$lambda)
  res <- semislv(1, 1, Phi_fn, Psi_fn, save = list(path = TRUE), alpha = 1)
  res <- unclass(res)
  expect_equal("run.time" %in% names(res), TRUE)
  expect_equal(length(res$res_path) > 0, TRUE)
  final_iterspace <- res$res_path[[res$step]]
  expect_equal(res$theta, final_iterspace$parameters$theta)
  expect_equal(res$lambda, final_iterspace$parameters$lambda)
})