# --- select_vars ---------------------------------------------------------------

test_that("dtGAP renders with select_vars subset", {
  skip_unless_extended()

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(
    dtGAP(
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      select_vars = c("LDH", "Lymphocyte"),
      print_eval = FALSE
    )
  )
})

test_that("dtGAP renders with single select_vars (drop = FALSE)", {
  skip_unless_extended()

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(
    dtGAP(
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      select_vars = "LDH",
      print_eval = FALSE
    )
  )
})

test_that("dtGAP errors on invalid select_vars names", {
  skip_unless_extended()

  expect_error(
    dtGAP(
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      select_vars = c("NO_SUCH_VAR")
    ),
    "not in the feature columns"
  )
})

test_that("dtGAP errors on empty select_vars", {
  skip_unless_extended()

  expect_error(
    dtGAP(
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      select_vars = character(0)
    ),
    "non-empty character vector"
  )
})

test_that("dtGAP errors on non-character select_vars", {
  skip_unless_extended()

  expect_error(
    dtGAP(
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      select_vars = 1:3
    ),
    "non-empty character vector"
  )
})
