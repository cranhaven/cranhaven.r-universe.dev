test_that("calculate_aic works as expected", {

  # setup
  x <- rnorm(100, mean = 0)
  y <- rpois(n = 100, lambda = exp(x + 1))
  dat <- data.frame(x = x, y = y)
  traditional <- glm(y ~ x, family = poisson)
  traditional2 <- lm(y ~ x)
  model <- glm_model(y ~ x, poisson)
  models <- list(
    poisson_model = glm_model(y ~ x, poisson),
    linear_model = lm_model(y ~ x)
  )
  expected_AIC <- AIC(traditional)
  expected_AIC2 <- AIC(traditional2)

  # single model tests
  calc_model <- evaluate_aic(model, dat)
  expect_equal(calc_model$result, expected_AIC)
  expect_equal(calc_model$metric, "aic")

  calc_model_tbl <- evaluate_aic(model, dat, as_tibble = TRUE)
  expect_equal(calc_model_tbl$result, expected_AIC)
  expect_equal(calc_model_tbl$metric, "aic")

  # multiple model tests
  calc_fitted_models <- evaluate_aic(models, dat)
  expect_equal(calc_fitted_models$result, c(expected_AIC, expected_AIC2))
  expect_equal(calc_fitted_models$metric, c("aic", "aic"))
  expect_equal(calc_fitted_models$model_name, c("poisson_model", "linear_model"))

  # should error
  expect_error(evaluate_aic("bob"))

})
