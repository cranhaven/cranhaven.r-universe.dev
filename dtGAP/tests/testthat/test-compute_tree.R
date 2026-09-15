# --- compute_tree -------------------------------------------------------------

test_that("compute_tree returns expected list structure", {
  fx <- build_classification_fixture()
  tr <- fx$tree_res

  expect_type(tr, "list")
  expect_named(tr, c("fit", "dat", "plot_data", "layout"))
})

test_that("compute_tree dat has required columns", {
  fx <- build_classification_fixture()
  dat <- fx$tree_res$dat

  expect_true("node_id" %in% names(dat))
  expect_true("y_hat" %in% names(dat))
  expect_true("Sample" %in% names(dat))
})

test_that("compute_tree plot_data has layout and nodesize columns", {
  fx <- build_classification_fixture()
  pd <- fx$tree_res$plot_data

  expect_true("x" %in% names(pd))
  expect_true("y" %in% names(pd))
  expect_true("recalculated_nodesize" %in% names(pd))
  expect_true("breaks_clean" %in% names(pd))
})

test_that("compute_tree layout has id, x, y", {
  fx <- build_classification_fixture()
  layout <- fx$tree_res$layout

  expect_true(all(c("id", "x", "y") %in% names(layout)))
  expect_true(all(layout$x >= 0 & layout$x <= 1))
  expect_true(all(layout$y >= 0 & layout$y <= 1))
})

# --- recalculate_nodesize -----------------------------------------------------

test_that("recalculate_nodesize sums children to parent", {
  fx <- build_classification_fixture()
  pd <- fx$tree_res$plot_data

  root <- pd[is.na(pd$parent), ]
  leaves <- pd[pd$kids == 0, ]
  expect_equal(root$recalculated_nodesize, sum(leaves$recalculated_nodesize))
})

# --- term_node_pos ------------------------------------------------------------

test_that("term_node_pos returns only leaf nodes", {
  fx <- build_classification_fixture()
  pd <- ggparty::ggparty(fx$fit)$data
  dat <- fx$tree_res$dat

  term <- term_node_pos(pd, dat)
  expect_true(all(term$kids == 0))
  expect_true("n" %in% names(term))
})

# --- prediction_df ------------------------------------------------------------

test_that("prediction_df returns data with predictions", {
  fx <- build_classification_fixture()
  dat <- fx$tree_res$dat

  expect_true(nrow(dat) > 0)
  expect_true("y_hat" %in% names(dat))
  expect_true("node_id" %in% names(dat))
  expect_true("Sample" %in% names(dat))
})

# --- compute_class_prob -------------------------------------------------------

test_that("compute_class_prob adds probability columns for rpart", {
  fx <- build_classification_fixture()
  pd <- fx$tree_res$plot_data

  prob_cols <- grep("^prob_", names(pd), value = TRUE)
  expect_true(length(prob_cols) > 0)

  # probabilities should be between 0 and 1
  for (col in prob_cols) {
    vals <- pd[[col]][!is.na(pd[[col]])]
    expect_true(all(vals >= 0 & vals <= 1))
  }
})

# --- position_nodes -----------------------------------------------------------

test_that("position_nodes returns layout for all nodes", {
  fx <- build_classification_fixture()
  layout <- fx$tree_res$layout
  pd <- ggparty::ggparty(fx$fit)$data

  expect_equal(nrow(layout), nrow(pd))
  expect_true(all(pd$id %in% layout$id))
})
