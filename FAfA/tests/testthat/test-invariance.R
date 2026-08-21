test_that("empty ordinal categories are identified within groups", {
  source_data <- data.frame(
    group = factor(c(rep("A", 8), rep("B", 6))),
    item = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 1, 2, 2, 3, 3)
  )

  result <- FAfA:::find_empty_ordinal_cells(source_data, "group", "item")

  expect_equal(nrow(result), 1L)
  expect_identical(result$Variable, "item")
  expect_identical(result$Group, "B")
  expect_identical(result$Empty_Categories, "4")
  expect_match(result$Frequencies, "4=0", fixed = TRUE)
})

test_that("empty ordinal categories are merged consistently without changing source data", {
  source_data <- data.frame(
    group = factor(c(rep("A", 8), rep("B", 6))),
    item = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 1, 2, 2, 3, 3)
  )
  original <- source_data

  result <- FAfA:::collapse_empty_ordinal_categories(
    source_data,
    "group",
    "item"
  )

  expect_identical(source_data, original)
  expect_equal(nrow(result$audit), 1L)
  expect_identical(result$audit$Original_Category, "4")
  expect_identical(result$audit$Recoded_Category, "3")
  expect_equal(
    nrow(FAfA:::find_empty_ordinal_cells(result$data, "group", "item")),
    0L
  )
})

test_that("measurement invariance LRT uses short model names", {
  set.seed(2026)
  sample_size <- 500L
  group <- factor(rep(c("A", "B"), each = sample_size / 2L))
  latent <- stats::rnorm(sample_size)
  source_data <- data.frame(
    group = group,
    item_1 = latent + stats::rnorm(sample_size, sd = 0.7),
    item_2 = latent + stats::rnorm(sample_size, sd = 0.7),
    item_3 = latent + stats::rnorm(sample_size, sd = 0.7),
    item_4 = latent + stats::rnorm(sample_size, sd = 0.7)
  )
  model <- "Factor =~ item_1 + item_2 + item_3 + item_4"
  configural <- lavaan::cfa(model, data = source_data, group = "group")
  metric <- lavaan::cfa(
    model,
    data = source_data,
    group = "group",
    group.equal = "loadings"
  )

  result <- FAfA:::measurement_invariance_lrt(list(
    configural = configural,
    metric = metric
  ))

  expect_identical(result$Model, c("Configural", "Metric"))
  expect_false(any(grepl("new\\(\\\"lavaan", result$Model)))
  expect_true(any(grepl("Chisq", names(result), fixed = TRUE)))
})

test_that("measurement invariance UI exposes ordinal category handling", {
  html <- as.character(inv_ui("inv"))

  expect_match(html, "inv-ordinal_empty_category_action", fixed = TRUE)
  expect_match(html, "inv-ordinal_diagnostics_table", fixed = TRUE)
  expect_match(html, "inv-ordinal_recode_table", fixed = TRUE)
})
