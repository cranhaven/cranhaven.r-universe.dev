test_that("`rf_domain_score()` works", {
  set.seed(123)
  library(dplyr)
  featured_col <- "cd_2022"
  train_data <- viral %>%
    dplyr::select(cd_2022, vl_2022)
  test_data <- sero 
  rf_hyperparameters <- list(mtry = 2, min_n = 5, trees = 500)
  threshold_value <- 0.99
  expect_snapshot(rf_domain_score(featured_col, train_data, rf_hyperparameters, test_data, threshold_value))
})