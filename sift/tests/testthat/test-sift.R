test_that("Test sift error handling", {
  expect_error(sift(df), regexp = "Must supply sift.col")
  expect_error(sift(df, b), regexp = "must be coercible to <numeric>")
  expect_error(sift(df, a, c(1, "a")), regexp = "scope must be coercible to <numeric>")
  expect_error(sift(df, a, c(1, NA)), regexp = "scope must not contain missing values")
})

test_that("Test sift empty output recognition", {
  expect_true(nrow(sift(df, a, 2, b == "D")) == 0)
  expect_true(nrow(sift(df, a, 2, is.na(a))) == 0)
})

test_that("Test sift produces expected output", {
  expect_equal(result$a, sort(result$a))
  expect_true(is_grouped_df(result2))
  expect_true(n_distinct(result2$b) == 1)
})
