test_that("ppc_summary computes mean and variance checks", {
  set.seed(1)

  mu_draws <- matrix(
    rnorm(200 * 5),
    nrow = 200,
    ncol = 5
  )

  fit <- list(
    y = rnorm(5),
    mu_draws = mu_draws
  )
  class(fit) <- "traffic_fit"

  ppc <- ppc_summary(fit, stats = c("mean", "var"))

  expect_s3_class(ppc, "traffic_ppc")
  expect_true(ppc$p_values$mean >= 0 && ppc$p_values$mean <= 1)
  expect_true(ppc$p_values$var >= 0 && ppc$p_values$var <= 1)
})
