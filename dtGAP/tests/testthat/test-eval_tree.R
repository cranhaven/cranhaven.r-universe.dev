# --- eval_tree ----------------------------------------------------------------

test_that("eval_tree returns list with expected elements", {
  fx <- build_classification_fixture()

  result <- eval_tree(
    x = "covid",
    fit = fx$fit,
    task = "classification",
    tree_res = fx$tree_res,
    target_lab = "Outcome",
    sorted_dat = fx$sorted_dat,
    show = "test",
    model = "rpart"
  )

  expect_type(result, "list")
  expect_named(result, c("data_info", "train_metrics", "test_metrics"))
})

test_that("eval_tree data_info contains dataset name and model", {
  fx <- build_classification_fixture()

  result <- eval_tree(
    x = "my_data",
    fit = fx$fit,
    task = "classification",
    tree_res = fx$tree_res,
    target_lab = "Outcome",
    sorted_dat = fx$sorted_dat,
    show = "test",
    model = "rpart"
  )

  expect_true(grepl("my_data", result$data_info))
  expect_true(grepl("rpart", result$data_info))
})

test_that("eval_tree show='test' returns test_metrics but not train_metrics", {
  fx <- build_classification_fixture()

  result <- eval_tree(
    x = "covid",
    fit = fx$fit,
    task = "classification",
    tree_res = fx$tree_res,
    target_lab = "Outcome",
    sorted_dat = fx$sorted_dat,
    show = "test",
    model = "rpart"
  )

  expect_null(result$train_metrics)
  expect_true(nchar(result$test_metrics) > 0)
})

test_that("eval_tree simple_metrics returns shorter output", {
  fx <- build_classification_fixture()

  full <- eval_tree(
    x = "covid",
    fit = fx$fit,
    task = "classification",
    tree_res = fx$tree_res,
    target_lab = "Outcome",
    sorted_dat = fx$sorted_dat,
    show = "test",
    model = "rpart",
    simple_metrics = FALSE
  )

  simple <- eval_tree(
    x = "covid",
    fit = fx$fit,
    task = "classification",
    tree_res = fx$tree_res,
    target_lab = "Outcome",
    sorted_dat = fx$sorted_dat,
    show = "test",
    model = "rpart",
    simple_metrics = TRUE
  )

  expect_true(nchar(simple$test_metrics) < nchar(full$test_metrics))
})

test_that("eval_tree rejects invalid seriate_method", {
  fx <- build_classification_fixture()

  expect_error(
    eval_tree(
      x = "covid",
      fit = fx$fit,
      task = "classification",
      tree_res = fx$tree_res,
      target_lab = "Outcome",
      sorted_dat = fx$sorted_dat,
      show = "test",
      model = "rpart",
      seriate_method = "INVALID"
    ),
    "seriate_method"
  )
})
