# --- interactive ---------------------------------------------------------------

test_that("interactive = TRUE errors without InteractiveComplexHeatmap", {
  skip_unless_extended()
  skip_if(
    requireNamespace("InteractiveComplexHeatmap", quietly = TRUE),
    "InteractiveComplexHeatmap is installed; cannot test missing-package error"
  )

  expect_error(
    dtGAP(
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      interactive = TRUE,
      print_eval = FALSE
    ),
    "InteractiveComplexHeatmap"
  )
})

test_that("interactive = FALSE still works normally", {
  skip_unless_extended()

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(
    dtGAP(
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      interactive = FALSE,
      print_eval = FALSE
    )
  )
})
