test_that("bglmar1() validates the backend before touching the disk", {
  # The expected text is produced by match.arg() in this same session, so the
  # test does not hard-code a base R message that translation or a new R
  # release could reword. The workbook does not exist, so an error naming the
  # backend also shows that validation happens before any file is read.
  reference <- function(backend = c("auto", "rstan", "cmdstanr")) match.arg(backend)
  expected <- tryCatch(reference("jags"), error = conditionMessage)
  expect_error(
    bglmar1(data_path = ec_missing_workbook(),
            circ_vars = ec_circ_simple,
            prod_vars = ec_prod_simple,
            backend = "jags"),
    expected,
    fixed = TRUE
  )
})

test_that("bglmar1() stops when no Stan backend is available", {
  skip_if(isNamespaceLoaded("rstan") || isNamespaceLoaded("cmdstanr"))
  # The input is read and validated before the backend is chosen, so the
  # workbook has to be valid for the backend check to be reached.
  # The packages that read and reshape the workbook are loaded first: once the
  # library path is hidden they could no longer be found, and the call would
  # stop before reaching the backend.
  for (ns in c("readxl", "dplyr", "tibble", "rlang")) loadNamespace(ns)
  # An option left behind by the user would change which backend is tried.
  old <- options(EconCausal.backend = NULL)
  on.exit(options(old), add = TRUE)
  expect_error(
    ec_without_libraries(
      bglmar1(data_path = ec_valid_workbook(),
              circ_vars = ec_circ_simple,
              prod_vars = ec_prod_simple)
    ),
    "Neither 'rstan' nor 'cmdstanr' is available",
    fixed = TRUE
  )
})

test_that("bglmar1() reads data_path even when an object named DATA exists", {
  # The object is placed where a user's session would hold it, the global
  # environment, and removed or restored afterwards. It is a valid table, while
  # the workbook has no date column, so only a call that reads data_path stops
  # with the temporal-column error; a call that used the object would go on
  # and return without error.
  had <- exists("DATA", envir = globalenv(), inherits = FALSE)
  if (had) previous <- get("DATA", envir = globalenv())
  on.exit(
    if (had) assign("DATA", previous, envir = globalenv()) else rm("DATA", envir = globalenv()),
    add = TRUE
  )
  assign("DATA", ec_valid_bglmar1_table(), envir = globalenv())
  expect_error(
    bglmar1(data_path = ec_foreign_workbook(),
            circ_vars = ec_circ_simple,
            prod_vars = ec_prod_simple),
    "No temporal column found",
    fixed = TRUE
  )
})
