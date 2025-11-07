test_that("transform_input works as expected", {
  input = list("A;var1", "B;var1", "C;var1")
  filter_element = transform_input(input)

  # Check if the column name is correctly extracted
  expect_equal(filter_element[[1]], "var1")

  # Check if the values are correctly extracted
  expect_equal(filter_element[[2]], list("A", "B", "C"))
})
