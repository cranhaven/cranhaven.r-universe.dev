test_that("survival prediction works", {
  mod <- readRDS(test_path("fixtures", "ln_fit_with_covariates.rds"))
  new_data <- data.frame(x = c("0", "1"))
  pred <- predict(mod, new_data, type = "survival", eval_time = c(20, 100), interval = "credible")
  
  expected <- structure(
    list(.pred = list(structure(list(
      .eval_time = c(20, 100),
      .pred_survival = c(0.886, 0.0253),
      .pred_lower = c(0.880, 0.0225),
      .pred_upper = c(0.891, 0.028)),
      row.names = c(NA, -2L), 
      class = c("tbl_df", "tbl", "data.frame")),
      structure(list(
        .eval_time = c(20, 100),
        .pred_survival = c(0.986, 0.565),
        .pred_lower = c(0.984, 0.555),
        .pred_upper = c(0.987, 0.575)),
        row.names = c(NA, -2L), 
        class = c("tbl_df", "tbl", "data.frame"))),
      strata = factor(c('x=0', 'x=1'))),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -2L)
  )
  
  expect_equal(pred, expected, tolerance = 0.5)
})

test_that("hazard prediction works", {
  mod <- readRDS(test_path("fixtures", "ln_fit_with_covariates.rds"))
  new_data <- data.frame(x = c("0", "1"))
  pred <- predict(mod, new_data = new_data, type = "hazard", eval_time = c(20, 100), interval = "credible")
  
  expected <- structure(list(.pred = list(structure(list(
    .eval_time = c(20, 100), 
    .pred_hazard = c(0.0148, 0.0363),
    .pred_lower = c(0.0142, 0.0348),
    .pred_upper = c(0.0153, 0.0381)), 
    row.names = c(NA, -2L), 
    class = c("tbl_df", "tbl", "data.frame")),
    structure(list(
      .eval_time = c(20, 100),
      .pred_hazard = c(0.00277, 0.00555),
      .pred_lower = c(0.00255, 0.00534),
      .pred_upper = c(0.00302, 0.00575)),
      row.names = c(NA, -2L),
      class = c("tbl_df", "tbl", "data.frame"))),
    strata = factor(c('x=0', 'x=1'))), 
    class = c("tbl_df", "tbl", "data.frame"), 
    row.names = c(NA, -2L))
  
  expect_equal(pred, expected, tolerance = 0.5)
})
