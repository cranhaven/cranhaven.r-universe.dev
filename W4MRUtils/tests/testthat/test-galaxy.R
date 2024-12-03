



test_that("Testing galaxy run process", {
  capture.output({
    testthat::expect_null(
      run_galaxy_processing(
        "test-tool",
        NULL
      )
    )
    testthat::expect_error(
      run_galaxy_processing(
        "test-tool",
        stop("error occured because.")
      ),
      "The tool test-tool - version unknown ended in error.",
      fixed = TRUE
    )
  }, type = "message")
})

test_that("Testing galaxy run process provides some variables", {
  out <- run_galaxy_processing(
    "test-tool",
    list(args = class(args), logger = class(logger)[[1]])
  )
  testthat::expect_equal(out$logger, "W4MLogger")
  testthat::expect_equal(out$args, "list")
})

test_that("Testing galaxy env detection", {
  testthat::expect_false(in_galaxy_env())
  for (env_var in c(
    "_GALAXY_JOB_HOME_DIR",
    "_GALAXY_JOB_TMP_DIR",
    "GALAXY_MEMORY_MB",
    "GALAXY_MEMORY_MB_PER_SLOT",
    "GALAXY_SLOTS"
  )) {
    env <- list()
    env[[env_var]] <- 1
    do.call(Sys.setenv, env)
    testthat::expect_true(in_galaxy_env())
    Sys.unsetenv(env_var)
  }
  testthat::expect_false(in_galaxy_env())
})

test_that("Testing galaxy run process in gx env", {
  Sys.setenv(`_GALAXY_JOB_HOME_DIR` = 1)
  out <- paste0(
    capture.output(
      testthat::expect_true(run_galaxy_processing(
        "test-tool",
        in_galaxy_env()
      )),
      type = "message"
    ),
    collapse = ""
  )
  Sys.unsetenv("_GALAXY_JOB_HOME_DIR")
  testthat::expect_true(any(grepl(
    "[\\s\\S]*Job starting time:[\\s\\S]*",
    out,
    perl = TRUE
  )))
  testthat::expect_true(any(grepl(
    "[\\s\\S]*R_HOME[\\s\\S]*",
    out,
    perl = TRUE
  )))
  testthat::expect_true(any(grepl(
    "[\\s\\S]*Parameters used in test-tool - version unknown[\\s\\S]*",
    out,
    perl = TRUE
  )))
  testthat::expect_true(any(grepl(
    "[\\s\\S]*End of 'test-tool' Galaxy module call[\\s\\S]*",
    out,
    perl = TRUE
  )))
})

test_that("Testing galaxy run process", {
  Sys.setenv(`_GALAXY_JOB_HOME_DIR` = 1)
  capture.output(
    args <- run_galaxy_processing(
      "test-tool",
      code = args,
      args = list(
        "a-integer" = "42",
        "a-float" = "3.14",
        "a-boolean" = "FALSE",
        "a-list" = "1,2,3",
        "a-__ob__mangled_param__cb____at__domain" = "__lt__3"
      )
    ),
    type = "message"
  )
  Sys.unsetenv("_GALAXY_JOB_HOME_DIR")
  testthat::expect_equal(
    args[["a-[mangled_param]@domain"]],
    "<3" ## kawaii
  )
})

test_that("Testing galaxy run function", {
  Sys.setenv(`_GALAXY_JOB_HOME_DIR` = 1)
  capture.output(
    args <- run_galaxy_function(
      "test-tool",
      func = function(args, logger) args,
      args = list(
        "a-integer" = "42",
        "a-float" = "3.14",
        "a-boolean" = "FALSE",
        "a-list" = "1,2,3",
        "a-__ob__mangled_param__cb____at__domain" = "__lt__3"
      )
    ), type = "message"
  )
  Sys.unsetenv("_GALAXY_JOB_HOME_DIR")
  testthat::expect_equal(
    args[["a-[mangled_param]@domain"]],
    "<3" ## kawaii
  )
})