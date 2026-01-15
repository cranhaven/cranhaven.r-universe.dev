test_that("interval is not supported for EM model", {
  mod <- readRDS(test_path("fixtures", "em_fit_with_covariates.rds"))
  expect_error(predict(mod, 
                       new_data = data.frame(x = c("0", "1")), 
                       type = "survival", 
                       eval_time = c(20, 100), 
                       interval = "credible"))
})

test_that("level is not supported for EM model", {
  mod <- readRDS(test_path("fixtures", "em_fit_with_covariates.rds"))
  expect_error(predict(mod, 
                       new_data = data.frame(x = c("0", "1")), 
                       type = "survival", 
                       eval_time = c(20, 100), 
                       level = 0.95))
})

test_that("survival prediction works", {
  mod <- readRDS(test_path("fixtures", "em_fit_with_covariates.rds"))
  new_data <- data.frame(x = c("0", "1"))
  
  pred <- predict(mod, new_data, type = "survival", eval_time = c(20, 100))
  
  expected <- structure(
    list(.pred = list(structure(list(
      .eval_time = c(20, 100),
      .pred_survival = c(0.9, 0.02)),
      row.names = c(NA, -2L), 
      class = c("tbl_df", "tbl", "data.frame")),
      structure(list(
        .eval_time = c(20, 100),
        .pred_survival = c(0.986, 0.6)),
        row.names = c(NA, -2L), 
        class = c("tbl_df", "tbl", "data.frame"))),
      strata = factor(c('x=0', 'x=1'))),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -2L)
  )
  
  expect_equal(pred, expected, tolerance = 1)
})

test_that("hazard prediction works", {
  mod <- readRDS(test_path("fixtures", "em_fit_with_covariates.rds"))
  new_data <- data.frame(x = c("0", "1"))
  pred <- predict(mod, new_data = new_data, type = "hazard", eval_time = c(20, 100))
  
  expected <- structure(list(.pred = list(structure(list(
    .eval_time = c(20, 100), 
    .pred_hazard = c(0.01, 0.04)), 
    row.names = c(NA, -2L), 
    class = c("tbl_df", "tbl", "data.frame")),
    structure(list(
      .eval_time = c(20, 100),
      .pred_hazard = c(0.002, 0.004)),
      row.names = c(NA, -2L),
      class = c("tbl_df", "tbl", "data.frame"))),
    strata = factor(c('x=0', 'x=1'))), 
    class = c("tbl_df", "tbl", "data.frame"), 
    row.names = c(NA, -2L))
  
  expect_equal(pred, expected, tolerance = 1)
})
