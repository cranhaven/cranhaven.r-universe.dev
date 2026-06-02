test_that("validate_inputs() returns TRUE for lipid_markers built-in lookup", {
  df <- data.frame(TC = 5, HDL_c = 1, TG = 1.3, LDL_c = 3)
  expect_true(validate_inputs(df,
    list(TC = "TC", HDL_c = "HDL_c", TG = "TG", LDL_c = "LDL_c"),
    fun_name = "lipid_markers"))
})

test_that("validate_inputs() returns TRUE for non-lipid fun_name without required_keys", {
  skip_on_cran()
  # Non-lipid fun_names fall through switch() to character(0) — no required
  # keys are checked, so any valid data.frame + col_map passes.
  df <- data.frame(G0 = 5.0, I0 = 10.0)
  expect_true(validate_inputs(df, list(G0 = "G0"), fun_name = "fasting_is"))
})

test_that("validate_inputs() uses explicit required_keys regardless of fun_name", {
  skip_on_cran()
  df <- data.frame(G0 = 5.0, I0 = 10.0)
  expect_true(validate_inputs(df,
    list(G0 = "G0", I0 = "I0"),
    fun_name = "fasting_is",
    required_keys = c("G0", "I0")))
})

test_that("validate_inputs() aborts when required key is missing from col_map", {
  skip_on_cran()
  df <- data.frame(G0 = 5.0)
  expect_error(
    validate_inputs(df, list(G0 = "G0"), fun_name = "fasting_is",
                    required_keys = c("G0", "I0")),
    "I0"
  )
})
