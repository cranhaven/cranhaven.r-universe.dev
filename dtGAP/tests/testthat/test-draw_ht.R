# --- prediction_annotation ----------------------------------------------------

test_that("prediction_annotation returns annotation and palettes", {
  fx <- build_classification_fixture()

  result <- prediction_annotation(
    sorted_dat = fx$sorted_dat,
    target_lab = "Outcome",
    task = "classification"
  )

  expect_type(result, "list")
  expect_named(result, c("annotation", "palettes"))
  expect_s4_class(result$annotation, "HeatmapAnnotation")
  expect_true("label_cols" %in% names(result$palettes))
})

test_that("prediction_annotation respects label_map", {
  fx <- build_classification_fixture()

  result <- prediction_annotation(
    sorted_dat = fx$sorted_dat,
    target_lab = "Outcome",
    task = "classification",
    label_map = c("0" = "Survival", "1" = "Death")
  )

  expect_true(all(c("Survival", "Death") %in% names(result$palettes$label_cols)))
})

test_that("prediction_annotation errors on invalid palette", {
  fx <- build_classification_fixture()

  expect_error(
    prediction_annotation(
      sorted_dat = fx$sorted_dat,
      target_lab = "Outcome",
      task = "classification",
      type_palette = "NonExistentPalette"
    )
  )
})

# --- row_prop_anno ------------------------------------------------------------

test_that("row_prop_anno returns annotation and palettes for numeric data", {
  fx <- build_classification_fixture()
  split_vec <- get_split_vec(fx$sorted_dat, fx$tree_res)
  layout <- compute_layout(fx$sorted_dat)

  result <- row_prop_anno(
    sorted_dat = fx$sorted_dat,
    layout = layout,
    split_vec = split_vec
  )

  expect_type(result, "list")
  expect_named(result, c("annotation", "palettes"))
})

test_that("row_prop_anno returns NULL annotation when row_pro_mat_sorted is NULL", {
  sorted_dat <- list(row_pro_mat_sorted = NULL)

  result <- row_prop_anno(
    sorted_dat = sorted_dat,
    layout = list(row_h = 100),
    split_vec = factor("1")
  )

  expect_null(result$annotation)
  expect_equal(length(result$palettes), 0)
})

# --- make_main_heatmap --------------------------------------------------------

test_that("make_main_heatmap returns heatmap and palettes", {
  fx <- build_classification_fixture()
  split_vec <- get_split_vec(fx$sorted_dat, fx$tree_res)
  layout <- compute_layout(fx$sorted_dat)

  pred_ha <- prediction_annotation(
    sorted_dat = fx$sorted_dat,
    target_lab = "Outcome",
    task = "classification"
  )

  row_ha <- row_prop_anno(
    sorted_dat = fx$sorted_dat,
    layout = layout,
    split_vec = split_vec
  )

  result <- make_main_heatmap(
    sorted_dat = fx$sorted_dat,
    split_vec = split_vec,
    pred_ha = pred_ha$annotation,
    row_prop_ha = row_ha$annotation,
    layout = layout
  )

  expect_type(result, "list")
  expect_named(result, c("heatmap", "palettes"))
  expect_s4_class(result$heatmap, "Heatmap")
  expect_true("col_mat" %in% names(result$palettes))
})

# --- col_ht -------------------------------------------------------------------

test_that("col_ht returns heatmap and palettes for numeric data", {
  fx <- build_classification_fixture()
  layout <- compute_layout(fx$sorted_dat)

  result <- col_ht(
    fit = fx$fit,
    sorted_dat = fx$sorted_dat,
    var_imp = fx$var_imp,
    layout = layout
  )

  expect_type(result, "list")
  expect_named(result, c("heatmap", "palettes"))
})

test_that("col_ht returns NULL heatmap when col_pro_mat_sorted is NULL", {
  fx <- build_classification_fixture()
  layout <- compute_layout(fx$sorted_dat)

  mod_sorted <- fx$sorted_dat
  mod_sorted$col_pro_mat_sorted <- NULL

  result <- col_ht(
    fit = fx$fit,
    sorted_dat = mod_sorted,
    var_imp = fx$var_imp,
    layout = layout
  )

  expect_null(result$heatmap)
  expect_equal(length(result$palettes), 0)
})

test_that("col_ht errors on invalid palette", {
  fx <- build_classification_fixture()
  layout <- compute_layout(fx$sorted_dat)

  expect_error(
    col_ht(
      fit = fx$fit,
      sorted_dat = fx$sorted_dat,
      var_imp = fx$var_imp,
      layout = layout,
      palette = "FakePalette"
    ),
    "Palette"
  )
})
