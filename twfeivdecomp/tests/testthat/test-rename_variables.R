# Tests for rename_variables() function

test_that("rename_variables renames columns correctly", {
  data <- data.frame(
    country = 1:3,
    year = c(2000, 2001, 2002),
    y = c(10, 20, 30),
    d = c(1, 0, 1),
    z = c(0, 1, 0)
  )

  renamed_data <- rename_variables(
    data = data,
    id_var = "country",
    time_var = "year",
    outcome_var = "y",
    treatment_var = "d",
    instrument_var = "z"
  )

  expect_true(all(c("id", "time", "outcome", "treatment", "instrument") %in% colnames(renamed_data)))
  expect_equal(renamed_data$id, data$country)
  expect_equal(renamed_data$time, data$year)
  expect_equal(renamed_data$outcome, data$y)
  expect_equal(renamed_data$treatment, data$d)
  expect_equal(renamed_data$instrument, data$z)
})
