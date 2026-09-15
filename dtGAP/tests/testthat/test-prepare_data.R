test_that("prepare_features converts target to factor for classification", {
  result <- prepare_features(iris, target_lab = "Species", task = "classification")
  expect_s3_class(result$Species, "factor")
  expect_s3_class(result, "tbl_df")
})

test_that("prepare_features converts character columns to factor", {
  df <- data.frame(
    x = 1:3,
    y = c("a", "b", "c"),
    target = c(1, 0, 1),
    stringsAsFactors = FALSE
  )
  result <- prepare_features(df, target_lab = "target", task = "classification")
  expect_s3_class(result$y, "factor")
  expect_s3_class(result$target, "factor")
})

test_that("prepare_features converts logical columns to Yes/No factor", {
  df <- data.frame(
    x = 1:3,
    flag = c(TRUE, FALSE, TRUE),
    target = c("A", "B", "A")
  )
  result <- prepare_features(df, target_lab = "target", task = "classification")
  expect_s3_class(result$flag, "factor")
  expect_setequal(levels(result$flag), c("No", "Yes"))
})

test_that("prepare_features errors on invalid target_lab", {
  expect_error(
    prepare_features(iris, target_lab = "nonexistent", task = "classification"),
    "target_lab"
  )
})

test_that("prepare_features errors on non-data.frame input", {
  expect_error(prepare_features(1:10), "data.frame")
})

test_that("add_data_type combines train and test", {
  result <- add_data_type(data_train = toy_train, data_test = toy_test)
  expect_true("data_type" %in% names(result))
  expect_equal(nrow(result), nrow(toy_train) + nrow(toy_test))
  expect_s3_class(result$data_type, "factor")
  expect_setequal(levels(result$data_type), c("train", "test"))
})

test_that("add_data_type splits data_all", {
  result <- add_data_type(data_all = iris, test_size = 0.3)
  expect_true("data_type" %in% names(result))
  expect_equal(nrow(result), nrow(iris))
  n_test <- sum(result$data_type == "test")
  expect_equal(n_test, floor(0.3 * nrow(iris)))
})

test_that("add_data_type errors when neither data_all nor both train/test provided", {
  expect_error(
    add_data_type(data_train = toy_train),
    "data_train and data_test"
  )
})
