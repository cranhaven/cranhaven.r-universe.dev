# Tests for create_exposed_unexposed_combinations() function

test_that("create_exposed_unexposed_combinations without controls includes all valid combinations", {
  data <- data.frame(cohort = c(2000, 2001, 999999))
  control_vars <- character(0)

  result <- create_exposed_unexposed_combinations(data, control_vars)

  expect_true(all(result$exposed_cohort != result$unexposed_cohort))
  expect_true(all(result$design_type %in% c("Exposed vs Unexposed", "Exposed vs Not Yet Exposed", "Exposed vs Exposed Shift")))
  expect_true(any(result$unexposed_cohort == 999999 & result$design_type == "Exposed vs Unexposed"))
})

test_that("create_exposed_unexposed_combinations with controls restricts some combinations", {
  data <- data.frame(cohort = c(2000, 2001, 999999))
  control_vars <- c("X1", "X2")

  result <- create_exposed_unexposed_combinations(data, control_vars)

  expect_true(all(result$unexposed_cohort > result$exposed_cohort))
  expect_true(all(result$design_type %in% c("Exposed vs Unexposed", "Early Exposed vs Later Exposed")))
  expect_true(any(result$unexposed_cohort == 999999 & result$design_type == "Exposed vs Unexposed"))
})
