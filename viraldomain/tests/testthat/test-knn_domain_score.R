test_that("`knn_domain_score()` works", {
  set.seed(123)
  library(dplyr)
  library(magrittr)
  featured_col <- "cd_2022"
  train_data = viral %>%
    dplyr::select(cd_2022, vl_2022)
  test_data = sero 
  knn_hyperparameters <- list(neighbors = 5, weight_func = "optimal", dist_power = 0.3304783)
  threshold_value <- 0.99
  expect_snapshot(knn_domain_score(featured_col, train_data, knn_hyperparameters, test_data, threshold_value))
})
