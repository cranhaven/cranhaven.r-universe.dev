test_that("filter_or", {
  filtered_data <- filter_all(
    iris,
    list(
      type = "OR",
      subfilter = list(
        list(
          type = "GT",
          value = 5,
          column = "Sepal.Length"
        ),
        list(
          type = "GT",
          value = 3,
          column = "Sepal.Width"
        )
      )
    )
  )

  result <- filtered_data$passed
  expect_true(all(result$Sepal.Length > 5 | result$Sepal.Width > 3))
})
