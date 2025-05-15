test_that("`glob_cr_vis()` plots as expected", {
  local_edition(3)
  library(dplyr)
  library(rules)
  library(Cubist)
  set.seed(123)
  hiv_data <- train2
  cr_hyperparameters <- list(neighbors = 5, committees = 58)
  vip_featured <- c("cd_2022")
  vip_features <- c("cd_2019", "vl_2019", "cd_2021", "vl_2021", "vl_2022")
  vip_train <- train2 |>
    dplyr::select(rsample::all_of(vip_features))
  vip_new <- vip_train[1, ]
  orderings <- 20
  v_train <- train2 |>
    dplyr::select(rsample::all_of(vip_featured))
  vdiffr::expect_doppelganger(
    title = "global cr vis",
    fig = glob_cr_vis(vip_featured, hiv_data, cr_hyperparameters, vip_train, v_train),
  )
})
