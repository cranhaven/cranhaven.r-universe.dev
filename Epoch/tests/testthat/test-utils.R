
# ---- tryToNum ----
test_that("tryToNum converts valid input", {
  expect_equal(tryToNum("123"), 123)
  expect_equal(tryToNum(c("1", "2", "3")), c(1, 2, 3))
  expect_equal(tryToNum(3.14), 3.14)
})

test_that("tryToNum returns NULL on invalid input", {
  expect_null(tryToNum("abc"))
  expect_null(tryToNum(c("1", "a", "3")))  # mixed input with invalid
})

# ---- isWholeNumber ----
test_that("isWholeNumber returns TRUE for integers", {
  expect_true(isWholeNumber(4))
})

test_that("isWholeNumber returns FALSE for non-integers", {
  expect_false(isWholeNumber(4.5))
  expect_equal(isWholeNumber(c(1, 2, 0.3, 3)), c(TRUE, TRUE, FALSE, TRUE))
  expect_error(isWholeNumber(TRUE), "is.numeric\\(x\\) is not TRUE")
})

#test_that("isWholeNumber handles tolerance", {
#  expect_true(isWholeNumber(2.00000001, tol = 1e-6))
#})

# ---- .message ----
test_that(".message prints when verbose is TRUE", {
  expect_message(.message(TRUE, "hello"))
})

test_that(".message does not print when verbose is FALSE", {
  expect_silent(.message(FALSE, "should not print"))
})

# ---- .checkIndex ----
test_that(".checkIndex works with numeric indices", {
  result <- .checkIndex(c(1, 3, 5), names = letters[1:5])
  expect_equal(result, c(1, 3, 5))
})

test_that(".checkIndex drops out-of-range numeric indices", {
  expect_warning({
    result <- .checkIndex(c(1, 6), names = letters[1:5])
  }, regexp = "Indices 6 are out of range")
  expect_equal(result, 1)
})

test_that(".checkIndex works with character indices", {
  result <- .checkIndex(c("a", "c"), names = letters[1:5])
  expect_equal(result, c(1, 3))
})

test_that(".checkIndex warns and drops invalid character indices", {
  expect_warning({
    result <- .checkIndex(c("a", "z"), names = letters[1:5])
  }, regexp = "Indices z are out of range")
  expect_equal(result, 1)
})

test_that(".checkIndex returns integer(0) for empty input", {
  expect_equal(.checkIndex(integer(0), names = letters), NULL)
  expect_equal(.checkIndex(1:3, names = character(0)), NULL)
})

# ---- .standardizeIEEG ----
test_that(".standardizeIEEG handles data frames", {
  df <- data.frame(a = c(1, 2, 3), b = c(3, 4, 5))
  result <- .standardizeIEEG(df)
  expect_type(result, "double")
  expect_equal(dim(result), dim(df))
})

test_that(".standardizeIEEG standardizes rows (Z-score)", {
  data <- matrix(c(1, 2, 3, 4,
                   2, 2, 2, 2), nrow = 2, byrow = TRUE)
  result <- .standardizeIEEG(data)
  
  expect_equal(round(rowMeans(result, na.rm = TRUE), 5), c(0,0))  # mean ≈ 0
  expect_equal(round(apply(result, 1, sd, na.rm = TRUE)[1],2), 0.56)  # sd = 1 for first row
  expect_equal(result[2, ], rep(0, 4))  # constant row handled as 0s
})
