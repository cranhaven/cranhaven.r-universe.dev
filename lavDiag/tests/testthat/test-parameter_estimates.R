test_that("parameter_estimates returns tidy table with group column", {
  fit <- .fit_mixed(n = 100)
  pe  <- parameter_estimates(fit)
  expect_s3_class(pe, "tbl_df")
  expect_true("group" %in% names(pe))
  expect_true(all(c("lhs","op","rhs","est") %in% names(pe)))
})

test_that("parameter_estimates standardized variant exposes unified est", {
  fit <- .fit_cont(n = 80)
  pes <- parameter_estimates(fit, standardized = TRUE)
  expect_s3_class(pes, "tbl_df")
  expect_true("est" %in% names(pes))
})
