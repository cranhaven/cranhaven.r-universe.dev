test_that("filter_not", {
  filtered_data <- filter_all(
    iris,
    list(
      type = "NOT",
      subfilter = list(
        type = "GT",
        value = 5,
        column = "Sepal.Length"
      )
    )
  )

  result <- filtered_data$passed
  expect_true(all(result$Sepal.Length <= 5))
})
