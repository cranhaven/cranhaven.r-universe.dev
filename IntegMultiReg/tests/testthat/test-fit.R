test_that("the fitted object has the expected class and structure", {
  expect_s3_class(fit_bin, "imr")
  for (nm in c("gam_mean", "theta_mean", "estimate_latent_y", "log_posterior",
               "gam_sample", "list_hyperpara", "data1", "data2",
               "model_bitstrings", "sample_size", "platform_names",
               "feature_names", "nu", "sample_mcmc")) {
    expect_true(nm %in% names(fit_bin), info = nm)
  }
  expect_equal(fit_bin$n_platform, 3L)
  expect_equal(length(fit_bin$gam_mean), 3L)
})

test_that("subgroup structure matches the simulated availability patterns", {
  expect_setequal(fit_bin$model_bitstrings, c("011", "100", "101", "111"))
  expect_equal(as.integer(fit_bin$sample_size[c("011", "100", "101", "111")]),
               c(120L, 60L, 60L, 60L))
})

test_that("inclusion probabilities are valid and dimensions are right", {
  mp <- coef(fit_bin)
  expect_equal(names(mp), c("genomic", "proteomic", "metabolomic"))
  expect_equal(ncol(mp$genomic), 20L)
  expect_equal(ncol(mp$proteomic), 10L)
  expect_equal(ncol(mp$metabolomic), 8L)
  for (m in mp) {
    expect_true(all(m >= 0 & m <= 1))
    expect_false(anyNA(m))
  }
})

test_that("the log-posterior trace is finite and the right length", {
  expect_length(fit_bin$log_posterior, sum(fit_bin$sample_mcmc))
  expect_true(all(is.finite(fit_bin$log_posterior)))
})

test_that("results are reproducible for a fixed seed", {
  a <- fit_demo("binary", total = 200, burn = 100, seed = 7)
  b <- fit_demo("binary", total = 200, burn = 100, seed = 7)
  expect_equal(a$gam_mean, b$gam_mean)
  expect_equal(a$log_posterior, b$log_posterior)
})

test_that("seed = NULL follows the ambient RNG (set.seed reproducibility)", {
  run <- function() {
    imr(simIMR$platforms, simIMR$outcome.binary, cov = simIMR$covariates,
        type_outcome = "binary", nu = c(-4, -3, -4),
        sample_mcmc = c(120, 60), ssize = 30, seed = NULL)
  }
  set.seed(123); a <- run()
  set.seed(123); b <- run()
  expect_equal(a$log_posterior, b$log_posterior)
})

test_that("the model recovers planted signal above null features", {
  # deterministic given the seed; use a moderate run
  fit <- fit_demo("continuous", total = 800, burn = 300, seed = 5)
  mp <- coef(fit)
  # genomic truth = G01,G02,G03; compare best true vs best null feature
  true_max <- max(apply(mp$genomic[, 1:3, drop = FALSE], 2, max))
  null_max <- max(apply(mp$genomic[, 8:20, drop = FALSE], 2, max))
  expect_gt(true_max, null_max)
  # at least one truly associated feature is confidently selected somewhere
  expect_gt(max(apply(mp$proteomic[, 1:2, drop = FALSE], 2, max)), 0.5)
})

test_that("BMS fits keep theta fixed at zero and return empty theta samples", {
  fit <- imr(
    simIMR$platforms, simIMR$outcome.binary, cov = simIMR$covariates,
    type_outcome = "binary", method = "BMS", nu = c(-4, -3, -4),
    sample_mcmc = c(80, 40), ssize = 30, seed = 16
  )

  expect_true(all(vapply(fit$theta_mean, function(x) all(x == 0), logical(1))))
  expect_true(all(vapply(fit$theta_sample, is.null, logical(1))))
})

test_that("single-subgroup fits handle zero-column theta samples", {
  set.seed(17)
  n <- 24
  platforms <- list(
    first = data.frame(id = seq_len(n), x1 = rnorm(n), x2 = rnorm(n)),
    second = data.frame(id = seq_len(n), z1 = rnorm(n))
  )
  outcome <- data.frame(id = seq_len(n), y = rnorm(n))

  fit <- imr(
    platforms, outcome, type_outcome = "continuous",
    sample_mcmc = c(12, 6), ssize = 5, seed = 17
  )

  expect_equal(fit$model_bitstrings, "11")
  expect_equal(dim(fit$theta_sample[[1]]), c(12L, 0L))
  expect_equal(dim(fit$theta_sample[[2]]), c(12L, 0L))
})
