# --- compare_dtGAP -------------------------------------------------------------

test_that("compare_dtGAP renders two models side-by-side", {
  skip_unless_extended()

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(
    compare_dtGAP(
      models = c("rpart", "rpart"),
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      print_eval = FALSE
    )
  )
})

test_that("compare_dtGAP errors with single model", {
  expect_error(
    compare_dtGAP(
      models = "rpart",
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome"
    ),
    "at least 2 models"
  )
})

test_that("compare_dtGAP errors with unsupported model", {
  expect_error(
    compare_dtGAP(
      models = c("rpart", "xgboost"),
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome"
    ),
    "Unsupported model"
  )
})
