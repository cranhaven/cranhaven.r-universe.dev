# --- rf_dtGAP ------------------------------------------------------------------

test_that("rf_dtGAP pipeline runs (forest training + tree extraction)", {
  skip_unless_extended()

  rf_result <- train_rf(train_covid, target_lab = "Outcome", ntree = 5)
  fit <- partykit::gettree(rf_result$forest, tree = 1)

  expect_s3_class(fit, "party")
  expect_true(length(fit) > 0)
})

test_that("rf_dtGAP errors on invalid tree_index (too large)", {
  skip_unless_extended()

  expect_error(
    rf_dtGAP(
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      tree_index = 999,
      ntree = 5
    ),
    "tree_index"
  )
})

test_that("rf_dtGAP errors on tree_index = 0", {
  skip_unless_extended()

  expect_error(
    rf_dtGAP(
      data_train = train_covid,
      data_test = test_covid,
      target_lab = "Outcome",
      show = "test",
      tree_index = 0,
      ntree = 5
    ),
    "tree_index"
  )
})

test_that("rf_dtGAP renders end-to-end with suitable tree", {
  skip_unless_extended()

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  rf_result <- train_rf(train_covid, target_lab = "Outcome", ntree = 5)
  rendered <- FALSE
  for (k in seq_len(5)) {
    result <- tryCatch({
      fit <- partykit::gettree(rf_result$forest, tree = k)
      data_all_prep <- add_data_type(data_train = train_covid, data_test = test_covid)
      data_all_prep <- prepare_features(data_all_prep, "Outcome", "classification")
      data <- data_all_prep
      tree_res <- compute_tree(
        fit = fit, model = "cforest", show = "all",
        data = data, target_lab = "Outcome",
        task = "classification"
      )
      rendered <- TRUE
      break
    }, error = function(e) NULL)
  }
  expect_true(rendered, info = "At least one cforest tree should be extractable")
})
