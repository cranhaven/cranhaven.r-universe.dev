testthat::context("Functionality: Similar Cases")

testthat::test_that("Survival Random Forest", {
  
})

testthat::test_that("Regression Random Forest", {
  
})

testthat::test_that("Classification Random Forest", {
  df = tibble::tibble(
    class = as.factor(c(rep(0, 100), rep(1, 100), rep(2, 100))),
    x1 = c(rnorm(100, 0, .1), rnorm(100, 10, .1), rnorm(100, 20, .1)),
    x2 = c(rnorm(100, 10, .1), rnorm(100, 0, .1), rnorm(100, 20, .1))
  )
  
  rf_model <- RFModel$new(class ~ ., data=df)
  df |>
    rf_model$fit()
  testthat::expect_is(rf_model$model_fit, 'ranger')
  
  rf_model$set_distance_method('Depth')
  d <- rf_model$calc_distance_matrix(df[, -1])
  testthat::expect_equal(sum(diag(table(cutree(hclust(d), k=3), df$class))), 300, info = 'Depth Distance')
  
  rf_model$set_distance_method('Proximity')
  d <- rf_model$calc_distance_matrix(df[, -1])
  testthat::expect_equal(sum(diag(table(cutree(hclust(d), k=3), df$class))), 300, info = 'Proximity Distance')
})

testthat::test_that("Linear Regression", {
  
})

testthat::test_that("Logistic Regression", {
  
})

testthat::test_that("Cox-Proportional-Hazard", {
  ovarian$resid.ds <- factor(ovarian$resid.ds)
  ovarian$rx <- factor(ovarian$rx)
  ovarian$ecog.ps <- factor(ovarian$ecog.ps)
  
  # initialize R6 object
  cph_model <- CoxModel$new(Surv(futime, fustat) ~ age + resid.ds + rx + ecog.ps, ovarian)
  
  ovarian |>
    cph_model$fit()
  testthat::expect_is(cph_model$model_fit, 'cph')
  
  ovarian %>%
    cph_model$calc_distance_matrix() -> dist_mat
  
  ovarian %>%
    cph_model$get_similar_cases(k = 3) -> matchedData
  
})