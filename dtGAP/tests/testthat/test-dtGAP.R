# --- dtGAP (integration) ------------------------------------------------------

test_that("dtGAP runs end-to-end for classification with train/test split", {
  skip_unless_extended()

  pdf(NULL) # suppress graphical output
  on.exit(dev.off(), add = TRUE)

  expect_no_error(
    dtGAP(
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      model = "rpart",
      print_eval = FALSE
    )
  )
})

test_that("dtGAP runs with label_map", {
  skip_unless_extended()

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(
    dtGAP(
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      label_map = c("0" = "Survival", "1" = "Death"),
      label_map_colors = c("Survival" = "#50046d", "Death" = "#fcc47f"),
      print_eval = FALSE
    )
  )
})

test_that("dtGAP runs with data_all and show='all'", {
  skip_unless_extended()

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(
    dtGAP(
      data_all = Psychosis_Disorder,
      model = "party",
      show = "all",
      trans_type = "none",
      target_lab = "UNIQID",
      print_eval = FALSE
    )
  )
})

test_that("dtGAP rejects invalid model argument", {
  expect_error(
    dtGAP(
      data_all = Psychosis_Disorder,
      target_lab = "UNIQID",
      model = "invalid_model"
    ),
    "arg"
  )
})

test_that("dtGAP rejects invalid seriate_method", {
  expect_error(
    dtGAP(
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      seriate_method = "NOT_A_METHOD"
    ),
    "seriate_method"
  )
})

test_that("dtGAP runs with simple_metrics", {
  skip_unless_extended()

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  expect_no_error(
    dtGAP(
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      simple_metrics = TRUE,
      print_eval = FALSE
    )
  )
})
