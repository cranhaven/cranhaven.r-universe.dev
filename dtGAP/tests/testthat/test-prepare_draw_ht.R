# --- heat_prop ----------------------------------------------------------------

test_that("heat_prop returns row and col proportions summing to 1", {
  fx <- build_classification_fixture()
  props <- heat_prop(fx$sorted_dat)

  expect_named(props, c("col", "row"))
  expect_equal(props$col + props$row, 1)
  expect_true(props$col > 0 && props$col < 1)
  expect_true(props$row > 0 && props$row < 1)
})

test_that("heat_prop proportions reflect matrix shape", {
  # more rows than cols → row_prop > col_prop
  sorted_dat <- list(sorted_test_matrix = matrix(0, nrow = 100, ncol = 10))
  props <- heat_prop(sorted_dat)
  expect_true(props$row > props$col)

  # more cols than rows → col_prop > row_prop
  sorted_dat2 <- list(sorted_test_matrix = matrix(0, nrow = 10, ncol = 100))
  props2 <- heat_prop(sorted_dat2)
  expect_true(props2$col > props2$row)
})

# --- compute_layout -----------------------------------------------------------

test_that("compute_layout returns expected structure", {
  fx <- build_classification_fixture()

  layout <- compute_layout(fx$sorted_dat)

  expect_type(layout, "list")
  expect_named(layout, c(
    "tree_w", "heatmap_w", "total_draw_h", "offset_h",
    "row_h", "col_h", "tree_h", "margin"
  ))
})

test_that("compute_layout tree_w + heatmap_w equals total_w", {
  fx <- build_classification_fixture()

  layout <- compute_layout(fx$sorted_dat, total_w = 297, tree_p = 0.3)
  expect_equal(layout$tree_w + layout$heatmap_w, 297)
})

test_that("compute_layout respects margin", {
  fx <- build_classification_fixture()

  layout <- compute_layout(fx$sorted_dat, margin = 40, total_h = 210)
  expect_equal(layout$total_draw_h, 170)
  expect_equal(layout$margin, 40)
})

test_that("compute_layout enforces minimum col_h of 10", {
  # Create a matrix with many rows, few cols → col_h would be tiny
  mat <- matrix(0, nrow = 500, ncol = 2)
  sorted_dat <- list(sorted_test_matrix = mat)

  layout <- compute_layout(sorted_dat, total_h = 210, margin = 20)
  expect_true(layout$col_h >= 10)
  expect_true(layout$offset_h > 0)
})

test_that("compute_layout tree_h equals row_h", {
  fx <- build_classification_fixture()

  layout <- compute_layout(fx$sorted_dat)
  expect_equal(layout$tree_h, layout$row_h)
})

# --- get_split_vec ------------------------------------------------------------

test_that("get_split_vec returns factor of leaf node ids", {
  fx <- build_classification_fixture()

  split_vec <- get_split_vec(
    sorted_dat = fx$sorted_dat,
    tree_res = fx$tree_res
  )

  expect_s3_class(split_vec, "factor")
  expect_equal(length(split_vec), nrow(fx$sorted_dat$sorted_test_matrix))

  # levels should be leaf node ids
  leaf_ids <- fx$tree_res$plot_data %>%
    dplyr::filter(kids == 0) %>%
    dplyr::pull(id) %>%
    as.character()
  expect_true(all(levels(split_vec) %in% leaf_ids))
})
