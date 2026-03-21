test_that("check no error has occurred", {
  # Create data
  data <- data.frame(a = 1:100, b = 1:100, c = 1:100, d = 1:100) / 10
  # Execute function
  expect_identical(sunflower(data), NULL)
})
