test_that("calculate_rmse works as expected", {

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
  expected_rmse <- yardstick::rmse_vec(dat$y, predict(traditional, type = "response"))
  expected_rmse2 <- yardstick::rmse_vec(dat$y, predict(traditional2))

  # single model tests
  calc_model <- calculate_rmse(model, dat)
  expect_equal(calc_model$result, expected_rmse)
  expect_equal(calc_model$metric, "rmse")

  calc_model_tbl <- calculate_rmse(model, dat, as_tibble = TRUE)
  expect_equal(calc_model_tbl$result, expected_rmse)
  expect_equal(calc_model_tbl$metric, "rmse")

  calc_fitted_model <- calculate_rmse(fitted_model)
  expect_equal(calc_fitted_model$result, expected_rmse)
  expect_equal(calc_fitted_model$metric, "rmse")

  calc_fitted_model_tbl <- calculate_rmse(fitted_model, as_tibble = TRUE)
  expect_equal(calc_fitted_model_tbl$result, expected_rmse)
  expect_equal(calc_fitted_model_tbl$metric, "rmse")

  calc_fitted_model2 <- calculate_rmse(fitted_model_tbl)
  expect_equal(calc_fitted_model2$result, expected_rmse)
  expect_equal(calc_fitted_model2$metric, "rmse")

  calc_pred_model <- calculate_rmse(pred)
  expect_equal(calc_pred_model$result, expected_rmse)
  expect_equal(calc_pred_model$metric, "rmse")

  calc_pred_tbl <- calculate_rmse(pred, as_tibble = TRUE)
  expect_equal(calc_pred_tbl$result, expected_rmse)
  expect_equal(calc_pred_tbl$metric, "rmse")

  # multiple model tests
  calc_fitted_models <- calculate_rmse(models, dat)
  expect_equal(calc_fitted_models$result, c(expected_rmse, expected_rmse2))
  expect_equal(calc_fitted_models$metric, c("rmse", "rmse"))
  expect_equal(calc_fitted_models$model_name, c("poisson_model", "linear_model"))
  calc_pred_model2 <- calculate_rmse(pred_models)
  expect_equal(calc_pred_model2$result, c(expected_rmse, expected_rmse2))
  expect_equal(calc_pred_model2$metric, c("rmse", "rmse"))

  # prediction test
  tmp <- calculate_rmse(pred, as_tibble = TRUE)
  expect_equal(tmp$result, expected_rmse)


  # should error
  expect_error(calculate_rmse(list("bob")))

})
