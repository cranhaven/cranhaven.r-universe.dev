test_that("train_tree returns fit and var_imp with rpart", {
  result <- train_tree(
    data_train = toy_train,
    target_lab = "Outcome",
    model = "rpart"
  )
  expect_named(result, c("fit", "var_imp"))
  expect_s3_class(result$fit, "party")
  expect_true(is.numeric(result$var_imp))
  expect_true(sum(result$var_imp) > 0)
})

test_that("train_tree var_imp sums to approximately 1", {
  result <- train_tree(
    data_train = toy_train,
    target_lab = "Outcome",
    model = "rpart"
  )
  expect_equal(sum(result$var_imp), 1, tolerance = 0.05)
})

test_that("train_tree errors when no data provided", {
  expect_error(train_tree(target_lab = "Outcome"), "provide")
})

test_that("train_tree can extract train from combined data", {
  data_all <- add_data_type(data_train = toy_train, data_test = toy_test)
  data_all <- prepare_features(data_all, target_lab = "Outcome", task = "classification")
  result <- train_tree(
    data = data_all,
    target_lab = "Outcome",
    model = "rpart"
  )
  expect_named(result, c("fit", "var_imp"))
})
