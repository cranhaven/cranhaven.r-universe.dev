# --- rf_summary ----------------------------------------------------------------

test_that("rf_summary renders and returns structure", {
  skip_unless_extended()

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  result <- rf_summary(
    data_train = train_covid,
    data_test = test_covid,
    target_lab = "Outcome",
    ntree = 5
  )

  expect_type(result, "list")
  expect_named(result, c("forest", "var_imp", "rep_tree_index"))
  expect_s3_class(result$forest, "cforest")
  expect_true(result$rep_tree_index >= 1)
  expect_true(result$rep_tree_index <= 5)
})

test_that("rf_summary works with show_rep_tree = FALSE", {
  skip_unless_extended()

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  result <- rf_summary(
    data_train = train_covid,
    data_test = test_covid,
    target_lab = "Outcome",
    ntree = 5,
    show_rep_tree = FALSE
  )

  expect_type(result, "list")
})
