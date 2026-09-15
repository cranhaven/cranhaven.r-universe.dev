# --- prepare_tree --------------------------------------------------------------

test_that("prepare_tree returns plot_data, branches, and branch_labels", {
  fx <- build_classification_fixture()

  result <- prepare_tree(fx$tree_res, model = "rpart")

  expect_type(result, "list")
  expect_named(result, c("plot_data", "branches", "branch_labels"))
})

test_that("prepare_tree plot_data has node_label column", {
  fx <- build_classification_fixture()

  result <- prepare_tree(fx$tree_res, model = "rpart")
  pd <- result$plot_data

  expect_true("node_label" %in% names(pd))
  expect_true("parent_label" %in% names(pd))
  expect_true("child_label" %in% names(pd))
  expect_true(all(!is.na(pd$node_label)))
})

test_that("prepare_tree branches has coordinate columns", {
  fx <- build_classification_fixture()

  result <- prepare_tree(fx$tree_res, model = "rpart")
  branches <- result$branches

  expect_true(all(c("x_start", "y_start", "x_end", "y_end") %in% names(branches)))
  expect_true(nrow(branches) > 0)
})

test_that("prepare_tree branch count equals number of non-root nodes", {
  fx <- build_classification_fixture()

  result <- prepare_tree(fx$tree_res, model = "rpart")
  pd <- result$plot_data
  n_children <- sum(!is.na(pd$parent))

  expect_equal(nrow(result$branches), n_children)
})

test_that("prepare_tree rpart labels contain probability info", {
  fx <- build_classification_fixture()

  result <- prepare_tree(fx$tree_res, model = "rpart")
  pd <- result$plot_data

  # Internal nodes should have splitvar and percentage
  internal <- pd[pd$kids > 0, ]
  expect_true(all(grepl("%", internal$node_label)))
})
