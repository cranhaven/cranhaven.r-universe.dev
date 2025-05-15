test_that("viralx_knn_glob() works", {
  local_edition(3)
  library(dplyr)
  set.seed(123)
  hiv_data <- train2
  knn_hyperparameters <- list(neighbors = 5, weight_func = "optimal", dist_power = 0.3304783)
  vip_featured <- c("cd_2022")
  vip_features <- c("cd_2019", "vl_2019", "cd_2021", "vl_2021", "vl_2022")
  vip_train <- train2 |>
    dplyr::select(rsample::all_of(vip_features))
  v_train <- train2 |>
    dplyr::select(rsample::all_of(vip_featured))
  expect_snapshot(print(viralx_knn_glob(vip_featured, hiv_data, knn_hyperparameters, vip_train, v_train)))
})
