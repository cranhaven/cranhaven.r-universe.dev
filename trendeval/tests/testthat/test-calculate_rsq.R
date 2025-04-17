test_that("calculate_rsq works as expected", {

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
  expected_rsq <- yardstick::rsq_vec(dat$y, predict(traditional, type = "response"))
  expected_rsq2 <- yardstick::rsq_vec(dat$y, predict(traditional2))

  # single model tests
  calc_model <- calculate_rsq(model, dat)
  expect_equal(calc_model$result, expected_rsq)
  expect_equal(calc_model$metric, "rsq")

  calc_model_tbl <- calculate_rsq(model, dat, as_tibble = TRUE)
  expect_equal(calc_model_tbl$result, expected_rsq)
  expect_equal(calc_model_tbl$metric, "rsq")

  calc_fitted_model <- calculate_rsq(fitted_model)
  expect_equal(calc_fitted_model$result, expected_rsq)
  expect_equal(calc_fitted_model$metric, "rsq")

  calc_fitted_model_tbl <- calculate_rsq(fitted_model, as_tibble = TRUE)
  expect_equal(calc_fitted_model_tbl$result, expected_rsq)
  expect_equal(calc_fitted_model_tbl$metric, "rsq")

  calc_fitted_model2 <- calculate_rsq(fitted_model_tbl)
  expect_equal(calc_fitted_model2$result, expected_rsq)
  expect_equal(calc_fitted_model2$metric, "rsq")

  calc_pred_model <- calculate_rsq(pred)
  expect_equal(calc_pred_model$result, expected_rsq)
  expect_equal(calc_pred_model$metric, "rsq")

  calc_pred_tbl <- calculate_rsq(pred, as_tibble = TRUE)
  expect_equal(calc_pred_tbl$result, expected_rsq)
  expect_equal(calc_pred_tbl$metric, "rsq")

  # multiple model tests
  calc_fitted_models <- calculate_rsq(models, dat)
  expect_equal(calc_fitted_models$result, c(expected_rsq, expected_rsq2))
  expect_equal(calc_fitted_models$metric, c("rsq", "rsq"))
  expect_equal(calc_fitted_models$model_name, c("poisson_model", "linear_model"))
  calc_pred_model2 <- calculate_rsq(pred_models)
  expect_equal(calc_pred_model2$result, c(expected_rsq, expected_rsq2))
  expect_equal(calc_pred_model2$metric, c("rsq", "rsq"))

  # prediction test
  tmp <- calculate_rsq(pred, as_tibble = TRUE)
  expect_equal(tmp$result, expected_rsq)


  # should error
  expect_error(calculate_rsq(list("bob")))

})
