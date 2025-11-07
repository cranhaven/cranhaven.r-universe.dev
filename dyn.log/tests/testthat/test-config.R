testthat::test_that(
  desc = "init_levels_works",
  code = {

    test_config_file <- system.file("test-data",
                                    "test-config.yaml",
                                    package = "dyn.log")

    config <- yaml::read_yaml(test_config_file,
                              eval.expr = TRUE)


    actual <- create_log_levels(config$levels)

    log_levels <- log_levels()

    expect_true(any(match(log_levels, "trace")))
    expect_true(any(match(log_levels, "debug")))
    expect_true(any(match(log_levels, "info")))
    expect_true(any(match(log_levels, "success")))
    expect_true(any(match(log_levels, "warn")))
    expect_true(any(match(log_levels, "error")))
    expect_true(any(match(log_levels, "critical")))
    expect_true(any(match(log_levels, "fatal")))

    expect_equal(length(actual), length(log_levels))
  }
)

testthat::test_that(
  desc = "init_settings_works",
  code = {

    test_config_file <- system.file("test-data",
                                    "test-config.yaml",
                                    package = "dyn.log")

    config <- yaml::read_yaml(test_config_file,
                              eval.expr = TRUE)

    apply_active_settings(config$settings)

    expect_equal(active$threshold$name, "TRACE")
    expect_equal(active$threshold$severity, 600)
    expect_true(!is.null(active$callstack))

    expect_equal(active$callstack$max, 5)
    expect_equal(active$callstack$start, -1)
    expect_equal(active$callstack$stop, -1)
  }
)

testthat::test_that(
  desc = "get_configurations_works",
  code = {

    log_configs <- get_configurations()

    expect_true(!is.null(log_configs$default))
    expect_true(!is.null(log_configs$knitr))
    expect_true(!is.null(log_configs$object))
  }
)

testthat::test_that(
  desc = "object_config_works",
  code = {

    test_config_file <- system.file("test-data",
                                    "test-object.yaml",
                                    package = "dyn.log")

    testthat::capture_output_lines({
      init_logger(file_path = test_config_file)
    })

    test_obj <- DerivedTestObject$new()

    actual <- capture_output_lines({
      test_obj$invoke_logger("Logger")
    })

    expect_equal(length(actual), 2)

    expect_true(stringr::str_detect(actual[1], stringr::fixed("Object Id:")))
    expect_true(stringr::str_detect(actual[1], stringr::fixed(test_obj$identifier())))

    expect_true(stringr::str_detect(actual[1], stringr::fixed("Class:")))
    expect_true(stringr::str_detect(actual[1], stringr::fixed(test_obj$class_name())))

    expect_true(stringr::str_detect(actual[2], stringr::fixed("TRACE")))
    expect_true(stringr::str_detect(actual[2], stringr::fixed("derived test - 321 - 200")))
  }
)

testthat::test_that(
  desc = "no_var_failes",
  code = {

    test_config_file <- system.file("test-data",
                                    "test-no-var.yaml",
                                    package = "dyn.log")
    expect_error({
      testthat::capture_output_lines({
        init_logger(file_path = test_config_file)
      })
    })
  }
)

testthat::test_that(
  desc = "options_variable_works",
  code = {

    test_config_file <- system.file("test-data",
                                    "test-object.yaml",
                                    package = "dyn.log")

    options("dyn.log.config" = test_config_file)

    config_specification()

    test_obj <- DerivedTestObject$new()

    actual <- capture_output_lines({
      test_obj$invoke_logger("Logger")
    })

    expect_equal(length(actual), 2)

    expect_true(stringr::str_detect(actual[1], stringr::fixed("Object Id:")))
    expect_true(stringr::str_detect(actual[1], stringr::fixed(test_obj$identifier())))

    expect_true(stringr::str_detect(actual[1], stringr::fixed("Class:")))
    expect_true(stringr::str_detect(actual[1], stringr::fixed(test_obj$class_name())))

    expect_true(stringr::str_detect(actual[2], stringr::fixed("TRACE")))
    expect_true(stringr::str_detect(actual[2], stringr::fixed("derived test - 321 - 200")))
  }
)

testthat::test_that(
  desc = "option_missing_warns",
  code = {

    options("dyn.log.config" = "does/exist.yaml")

    expect_warning({
      config_specification()
    })
  }
)

testthat::test_that(
  desc = "variable_name_works",
  code = {

    test_config_file <- system.file("test-data",
                                    "test-variable.yaml",
                                    package = "dyn.log")

    config <- yaml::read_yaml(test_config_file, eval.expr = TRUE)

    ensure_logger(config$variable)

    idx <- which(ls(globalenv()) == config$variable)

    expect_gt(idx, 0)
  }
)

testthat::test_that(
  desc = "wipe_works",
  code = {

    test_config_file <- system.file("test-data",
                                    "test-variable.yaml",
                                    package = "dyn.log")

    config <- yaml::read_yaml(test_config_file, eval.expr = TRUE)

    ensure_logger(config$variable)

    idx <- which(ls(globalenv()) == config$variable)

    expect_gt(idx, 0)

    wipe_logger()

    idx <- which(ls(globalenv()) == config$variable)

    expect_true(identical(idx, integer()))
  }
)

testthat::test_that(
  desc = "log_levels_display",
  code = {

    test_config_file <- system.file("test-data",
                                    "test-config.yaml",
                                    package = "dyn.log")

    testthat::capture_output_lines({
      init_logger(file_path = test_config_file)
    })

    actual <- capture_output_lines({
      display_log_levels()
    })

    for (level in log_levels()) {
      info <- level_info(level)
      pattern <- paste(info$name, info$description)

      expect_true(any(!is.na(match(actual, pattern))), label = info$name)
    }
  }
)
