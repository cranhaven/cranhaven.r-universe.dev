test_that("critical interface flow remains available", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if(
    !identical(Sys.getenv("FAFA_RUN_SHINYTEST2"), "true"),
    "Set FAFA_RUN_SHINYTEST2=true to run browser tests."
  )

  app_dir <- system.file("app", package = "FAfA")
  data_file <- system.file("extdata", "ui-test-data.csv", package = "FAfA")
  app <- shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "critical-flow",
    seed = 1234,
    load_timeout = 120000
  )
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(tabs = "data")
  app$upload_file(`data_selection-file1` = data_file)
  app$set_inputs(`data_selection-analyze_data` = "click")
  app$wait_for_idle()

  expect_false(is.null(app$get_value(output = "data_selection-n_var_box")))

  app$set_inputs(tabs = "recode_var")
  app$set_inputs(`wrangling_recode-reverse_variables` = "item_2")
  app$set_inputs(`wrangling_recode-apply_reverse_scoring` = "click")
  app$wait_for_idle()
  expect_match(
    app$get_value(output = "wrangling_recode-recode_status"),
    "1 item"
  )

  app$set_inputs(tabs = "project")
  app$set_inputs(`project-app_language` = "tr")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "project-app_language"), "tr")
})
