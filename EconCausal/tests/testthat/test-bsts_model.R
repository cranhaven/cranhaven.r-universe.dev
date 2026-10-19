# The first test must stay first: it hides the libraries to reach the guard, and
# that only works while 'bsts' is not loaded. Every later test in this file
# loads it, which is why the guard for 'BoomSpikeSlab' is checked statically.

test_that("bsts_model() names 'bsts' when that package is missing", {
  skip_if(isNamespaceLoaded("bsts"))
  expect_error(
    ec_without_libraries(
      bsts_model(data_path = ec_missing_workbook(),
                 circ_vars = ec_circ_simple,
                 prod_vars = ec_prod_simple)
    ),
    "Package 'bsts' is required by bsts_model()",
    fixed = TRUE
  )
})

test_that("bsts_model() also guards 'BoomSpikeSlab'", {
  # Loading 'bsts' always loads 'BoomSpikeSlab', so no run-time state has the
  # first package present and the second one missing. The contract that both
  # guards exist is therefore read off the installed function itself.
  src <- ec_source_of(bsts_model)
  expect_match(src, 'requireNamespace("BoomSpikeSlab", quietly = TRUE)', fixed = TRUE)
  expect_match(src, "Package 'BoomSpikeSlab' is required by bsts_model()", fixed = TRUE)
})

test_that("bsts_model() aborts when the workbook does not exist", {
  skip_if_not_installed("bsts")
  skip_if_not_installed("BoomSpikeSlab")
  expect_error(
    bsts_model(data_path = ec_missing_workbook(),
               circ_vars = ec_circ_simple,
               prod_vars = ec_prod_simple)
  )
})

test_that("bsts_model() requires a temporal column", {
  skip_if_not_installed("bsts")
  skip_if_not_installed("BoomSpikeSlab")
  expect_error(
    bsts_model(data_path = ec_foreign_workbook(),
               circ_vars = ec_circ_simple,
               prod_vars = ec_prod_simple),
    "No temporal column found",
    fixed = TRUE
  )
})

test_that("bsts_model() leaves global options untouched after failing", {
  skip_if_not_installed("bsts")
  skip_if_not_installed("BoomSpikeSlab")
  # scipen is the option the function overwrites, and 77 is a value it never
  # sets, so finding it afterwards means the saved options were restored.
  old <- options(scipen = 77L)
  on.exit(options(old), add = TRUE)
  expect_error(
    bsts_model(data_path = ec_missing_workbook(),
               circ_vars = ec_circ_simple,
               prod_vars = ec_prod_simple)
  )
  expect_identical(getOption("scipen"), 77L)
})
