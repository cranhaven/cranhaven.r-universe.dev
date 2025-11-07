test_that("test_log_single_instance", {

  inst_n <- LogDispatch$new()
  inst_m <- LogDispatch$new()

  expect_true(identical(inst_n, inst_m))
})

test_that("test_default_log_dispatch_works", {
  log <- LogDispatch$new()

  output <- capture_output_lines({
    log$trace("test")
  })

  expect_gt(length(output), 0)
  expect_gt(nchar(output), 0)
})

test_that("test_log_threshold_works", {

  logger <- LogDispatch$new()

  active$threshold$name <- "INFO"
  active$threshold$severity <- 400

  actual <- capture_output({
    var1 <- "abc"; var2 <- 123; var3 <- 0.7535651
    logger$trace("log msg local vars: {var1}, {var2}, {var3}")
  })

  active$threshold$name <- "TRACE"
  active$threshold$severity <- 600

  expect_equal(actual, "")
  expect_false(stringr::str_detect(actual, stringr::fixed("TEST ")))
  expect_false(stringr::str_detect(actual, stringr::fixed("log msg local vars: abc, 123, 0.7535651")))
})

test_that("test_add_log_level_works", {

  test_config_file <- system.file("test-data",
                                  "test-config.yaml",
                                  package = "dyn.log")

  testthat::capture_output_lines({
    init_logger(file_path = test_config_file)
  })

  new_level <- new_log_level(
    name = "TEST",
    description = "for testing",
    severity = 42L,
    log_style = crayon::blue,
    msg_style = crayon::silver
  )

  dispatch <- LogDispatchTester$new()
  dispatch$attach_log_level(new_level)

  actual <- capture_output({
    var1 <- "abc"; var2 <- 123; var3 <- 0.7535651
    dispatch$test("log msg local vars: {var1}, {var2}, {var3}")
  })

  expect_true(stringr::str_detect(actual, stringr::fixed("TEST ")))
  expect_true(stringr::str_detect(actual, stringr::fixed("log msg local vars: abc, 123, 0.7535651")))

  # remove the log level
  log_levels(name = "test", level = NA)

  all_levels <- log_levels()
  expect_true(all(is.na(match(all_levels, tolower(level_name(new_level))))))
})

test_that("test_threshold_evaluation_works", {

  test_config_file <- system.file("test-data",
                                  "test-config.yaml",
                                  package = "dyn.log")

  testthat::capture_output_lines({
    init_logger(file_path = test_config_file)
  })

  new_level <- new_log_level(name = "TEST",
                       description = "for testing",
                       severity = 100L,
                       log_style = crayon::make_style("deepskyblue2")$bold,
                       msg_style = crayon::silver)

  dispatch <- LogDispatchTester$new()
  dispatch$attach_log_level(new_level)

  log_fn <- dispatch[["test"]]

  expect_true(!is.null(log_fn))

  actual <- capture_output({
    var1 <- "abc"; var2 <- 123; var3 <- 0.7535651
    log_fn("log msg local vars: {var1}, {var2}, {var3}")
  })

  expect_true(stringr::str_detect(actual, "TEST "))

  # remove the log level
  log_levels(name = "test", level = NA)

  all_levels <- log_levels()
  expect_true(all(is.na(match(all_levels, tolower(level_name(new_level))))))
})
