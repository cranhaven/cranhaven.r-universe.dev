# Tests for extract_variable_names() function

test_that("extract_variable_names extracts all variable types correctly", {
  f <- Y ~ D + X1 + X2 | X1 + X2 + Z
  result <- extract_variable_names(f)

  expect_type(result, "list")
  expect_equal(result$outcome_var, "Y")
  expect_equal(result$treatment_var, "D")
  expect_equal(result$instrument_var, "Z")
  expect_equal(result$control_vars, c("X1", "X2"))
}) 

test_that("extract_variable_names works when there are no controls", {
  f <- Y ~ D | Z
  result <- extract_variable_names(f)
  
  expect_type(result, "list")
  expect_equal(result$outcome_var, "Y")
  expect_equal(result$treatment_var, "D")
  expect_equal(result$instrument_var, "Z")
  expect_equal(result$control_vars, character(0))  
})


