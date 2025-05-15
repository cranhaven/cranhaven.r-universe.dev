test_that("`viralx_knn_vis()` works", {
  local_edition(3)
  set.seed(123)
  hiv_data <- train2
  knn_hyperparameters <- list(neighbors = 5, weight_func = "optimal", dist_power = 0.3304783)
  vip_featured <- c("cd_2022")
  vip_train <- hiv_data
  vip_new <- vip_train[1, ]
  orderings <- 20
  vdiffr::expect_doppelganger(
    title = "viralx_knn_vis",
    fig = viralx_knn_vis(vip_featured, hiv_data, knn_hyperparameters, vip_train, vip_new, orderings)
  )
})
