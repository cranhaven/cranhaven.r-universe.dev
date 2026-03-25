test_that("filter_greater_than_equal", {
  filtered_data <- filter_all(
    iris,
    list(
      type = "GT_EQ",
      value = 5,
      column = "Sepal.Length"
    )
  )
  expect_true(all(filtered_data$passed$Sepal.Length >= 5))
})
