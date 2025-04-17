test_that("calculate_mae works as expected", {

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
  fitted_model <- trending::fit(model, dat)
  fitted_model_tbl <- trending::fit(model, dat, as_tibble = TRUE)
  fitted_models <- trending::fit(models, dat)
  pred <- predict(model, dat)
  pred_models <- predict(models, dat)
  expected_mae <- yardstick::mae_vec(dat$y, predict(traditional, type = "response"))
  expected_mae2 <- yardstick::mae_vec(dat$y, predict(traditional2))

  # single model tests
  calc_model <- calculate_mae(model, dat)
  expect_equal(calc_model$result, expected_mae)
  expect_equal(calc_model$metric, "mae")

  calc_model_tbl <- calculate_mae(model, dat, as_tibble = TRUE)
  expect_equal(calc_model_tbl$result, expected_mae)
  expect_equal(calc_model_tbl$metric, "mae")

  calc_fitted_model <- calculate_mae(fitted_model)
  expect_equal(calc_fitted_model$result, expected_mae)
  expect_equal(calc_fitted_model$metric, "mae")

  calc_fitted_model_tbl <- calculate_mae(fitted_model, as_tibble = TRUE)
  expect_equal(calc_fitted_model_tbl$result, expected_mae)
  expect_equal(calc_fitted_model_tbl$metric, "mae")

  calc_fitted_model2 <- calculate_mae(fitted_model_tbl)
  expect_equal(calc_fitted_model2$result, expected_mae)
  expect_equal(calc_fitted_model2$metric, "mae")

  calc_pred_model <- calculate_mae(pred)
  expect_equal(calc_pred_model$result, expected_mae)
  expect_equal(calc_pred_model$metric, "mae")

  calc_pred_tbl <- calculate_mae(pred, as_tibble = TRUE)
  expect_equal(calc_pred_tbl$result, expected_mae)
  expect_equal(calc_pred_tbl$metric, "mae")

  # multiple model tests
  calc_fitted_models <- calculate_mae(models, dat)
  expect_equal(calc_fitted_models$result, c(expected_mae, expected_mae2))
  expect_equal(calc_fitted_models$metric, c("mae", "mae"))
  expect_equal(calc_fitted_models$model_name, c("poisson_model", "linear_model"))
  calc_pred_model2 <- calculate_mae(pred_models)
  expect_equal(calc_pred_model2$result, c(expected_mae, expected_mae2))
  expect_equal(calc_pred_model2$metric, c("mae", "mae"))

  # prediction test
  tmp <- calculate_mae(pred, as_tibble = TRUE)
  expect_equal(tmp$result, expected_mae)


  # should error
  expect_error(calculate_mae(list("bob")))

})
