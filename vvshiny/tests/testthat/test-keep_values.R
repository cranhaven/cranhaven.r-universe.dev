test_that("keep_values works as expected", {
  input = c("A;var1", "B;var1", "C;var1")
  values = keep_values(input)

  # Check if the values are correctly extracted
  expect_equal(values, list("A", "B", "C"))
})
