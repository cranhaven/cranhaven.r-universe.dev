# Test case 1: Both vectors are empty
test_that("test_reasonable returns NaN when both vectors are empty", {
  result1 <- test_reasonable(c(), c())
  expect_equal(result1, NaN)
})

# Test case 2: One vector is empty, the other has values greater than or equal to 1
test_that("test_reasonable returns 0 when one vector is empty and the other has values > 1", {
  result2 <- test_reasonable(c(), c(2, 3, 4))
  expect_equal(result2, 0.5)
})

# Test case 3: One vector is empty, the other has values less than 1
test_that("test_reasonable returns -3 when one vector is empty and the other has values < 1", {
  result3 <- test_reasonable(c(), c(0.5, 0.2, 0.7))
  expect_equal(result3, -3)
})

# Test case 4: Both vectors have values greater than or equal to 1
test_that("test_reasonable returns the correct result when both vectors have values >= 1", {
  result4 <- test_reasonable(c(2, 3, 1), c(1, 2, 4))
  expect_equal(result4, 0.25)
})

# Test case 5: Both vectors have values less than 1
test_that("test_reasonable returns -6 when both vectors have values < 1", {
  result5 <- test_reasonable(c(0.2, 0.5, 0.7), c(0.4, 0.6, 0.3))
  expect_equal(result5, -6)
})
