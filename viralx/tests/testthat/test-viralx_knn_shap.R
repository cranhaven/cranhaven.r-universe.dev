test_that("`viralx_knn_shap()` works", {
  local_edition(3)
  set.seed(123)
  hiv_data <- train2
  knn_hyperparameters <- list(neighbors = 5, weight_func = "optimal", dist_power = 0.3304783)
  vip_featured <- c("cd_2022")
  vip_train <- hiv_data
  vip_new <- vip_train[1, ]
  orderings <- 20
  expect_snapshot(print(viralx_knn_shap(vip_featured, hiv_data, knn_hyperparameters, vip_train, vip_new, orderings)))
})
