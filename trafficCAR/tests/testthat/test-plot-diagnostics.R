

test_that("plot_mcmc_diagnostics returns a list with plot + summary", {
  fit <- structure(
    list(draws = list(mu = rnorm(50), tau = rnorm(50))),
    class = "traffic_fit"
  )

  out <- suppressWarnings(plot_mcmc_diagnostics(fit))

  expect_type(out, "list")
  expect_true(all(c("plot", "summary") %in% names(out)))

  expect_s3_class(out$plot, "ggplot")
  expect_s3_class(out$summary, "data.frame")

  expect_true(all(c("parameter", "ess") %in% names(out$summary)))
  expect_type(out$summary$parameter, "character")
  expect_type(out$summary$ess, "double")

  expect_equal(nrow(out$summary), length(fit$draws))
  expect_setequal(out$summary$parameter, names(fit$draws))

  # ESS should be finite and nonnegative for typical numeric draws
  expect_true(all(is.finite(out$summary$ess)))
  expect_true(all(out$summary$ess >= 0))
})

test_that("plot_mcmc_diagnostics is deterministic for fixed inputs", {
  set.seed(123)
  fit <- structure(
    list(draws = list(a = rnorm(100), b = rnorm(100))),
    class = "traffic_fit"
  )

  out1 <- suppressWarnings(plot_mcmc_diagnostics(fit))
  out2 <- suppressWarnings(plot_mcmc_diagnostics(fit))

  expect_identical(out1, out2)
})

test_that("plot_mcmc_diagnostics handles empty draws", {
  fit <- structure(list(draws = list()), class = "traffic_fit")

  out <- suppressWarnings(plot_mcmc_diagnostics(fit))

  expect_type(out, "list")
  expect_true(all(c("plot", "summary") %in% names(out)))

  expect_s3_class(out$plot, "ggplot")
  expect_s3_class(out$summary, "data.frame")

  expect_true(all(c("parameter", "ess") %in% names(out$summary)))
  expect_equal(nrow(out$summary), 0)
  expect_equal(length(out$summary$parameter), 0)
  expect_equal(length(out$summary$ess), 0)
})

test_that("plot_mcmc_diagnostics works with a single parameter", {
  fit <- structure(list(draws = list(mu = rnorm(80))), class = "traffic_fit")

  out <- suppressWarnings(plot_mcmc_diagnostics(fit))

  expect_equal(nrow(out$summary), 1)
  expect_identical(out$summary$parameter, "mu")
  expect_true(is.finite(out$summary$ess))
  expect_true(out$summary$ess >= 0)
})

test_that("plot_mcmc_diagnostics rejects missing draws element", {
  fit <- structure(list(), class = "traffic_fit")
  expect_error(
    plot_mcmc_diagnostics(fit),
    "`fit\\$draws` is missing|draws",
    ignore.case = TRUE
  )
})

test_that("plot_mcmc_diagnostics errors when draws is not a list", {
  fit <- structure(list(draws = rnorm(10)), class = "traffic_fit")
  expect_error(plot_mcmc_diagnostics(fit), "must be a list", ignore.case = TRUE)
})

test_that("plot_mcmc_diagnostics requires posterior", {
  skip_if_not_installed("posterior")

  fit <- structure(list(draws = list(mu = rnorm(10))), class = "traffic_fit")
  expect_silent(suppressWarnings(plot_mcmc_diagnostics(fit)))
})

test_that("plot_mcmc_diagnostics errors on non-numeric draws", {
  fit <- structure(list(draws = list(mu = letters[1:10])), class = "traffic_fit")
  expect_error(plot_mcmc_diagnostics(fit), "numeric|finite", ignore.case = TRUE)
})

test_that("plot_mcmc_diagnostics errors on non-finite values (NA/NaN/Inf)", {
  fit1 <- structure(list(draws = list(mu = c(rnorm(20), NA_real_))), class = "traffic_fit")
  fit2 <- structure(list(draws = list(mu = c(rnorm(20), NaN))), class = "traffic_fit")
  fit3 <- structure(list(draws = list(mu = c(rnorm(20), Inf))), class = "traffic_fit")

  expect_error(plot_mcmc_diagnostics(fit1), "finite", ignore.case = TRUE)
  expect_error(plot_mcmc_diagnostics(fit2), "finite", ignore.case = TRUE)
  expect_error(plot_mcmc_diagnostics(fit3), "finite", ignore.case = TRUE)
})

test_that("plot_mcmc_diagnostics handles constant draws (may yield NA ESS)", {
  fit <- structure(list(draws = list(mu = rep(1, 200))), class = "traffic_fit")

  out <- suppressWarnings(plot_mcmc_diagnostics(fit))

  expect_equal(nrow(out$summary), 1)
  expect_identical(out$summary$parameter, "mu")

  # posterior::ess_basic may return NA for constant chains; that's acceptable.
  expect_true(is.na(out$summary$ess) || (is.finite(out$summary$ess) && out$summary$ess >= 0))
})

test_that("plot_mcmc_diagnostics handles very short chains (may yield NA ESS)", {
  fit <- structure(list(draws = list(mu = 1.0, tau = c(0.1, 0.2))), class = "traffic_fit")

  out <- suppressWarnings(plot_mcmc_diagnostics(fit))

  expect_equal(nrow(out$summary), 2)

  ok <- is.na(out$summary$ess) | (is.finite(out$summary$ess) & out$summary$ess >= 0)
  expect_true(all(ok))
})

test_that("plot_mcmc_diagnostics supports matrix draws per parameter", {
  mat <- matrix(rnorm(400), nrow = 200, ncol = 2)
  fit <- structure(list(draws = list(mu = mat)), class = "traffic_fit")

  out <- suppressWarnings(plot_mcmc_diagnostics(fit))

  expect_equal(nrow(out$summary), 1)
  expect_identical(out$summary$parameter, "mu")
  expect_true(is.finite(out$summary$ess))
  expect_true(out$summary$ess >= 0)
})

test_that("plot_mcmc_diagnostics scales to many parameters w/o crashing", {
  draws <- setNames(
    replicate(200, rnorm(50), simplify = FALSE),
    paste0("p", seq_len(200))
  )
  fit <- structure(list(draws = draws), class = "traffic_fit")

  out <- suppressWarnings(plot_mcmc_diagnostics(fit))

  expect_equal(nrow(out$summary), 200)
  expect_setequal(out$summary$parameter, names(draws))

  expect_true(all(is.finite(out$summary$ess)))
  expect_true(all(out$summary$ess >= 0))
})
