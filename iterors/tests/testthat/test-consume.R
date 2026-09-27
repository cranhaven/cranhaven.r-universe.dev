test_that("consume consumes an iterator when n < length(iterator)", {
  it <- iteror(letters)
  consume(it, n=4)
  expect_equal(nextOr(it, NA), "e")
})

test_that("consume consumes an iterator when n=0 or n > length(iterator)", {
  it <- iteror(letters)
  consume(it, n=Inf)
  expect_equal(nextOr(it, "stop"), "stop")

  it2 <- iteror(letters)
  consume(it2, n=27)
  expect_equal(nextOr(it, "stop"), "stop")
})

test_that("consume rejects non-positive or non-integer n", {
  it <- iteror(letters)
  expect_error(consume(it, -1), "n must be a non-negative integer of length 1")
  expect_error(consume(it, "a"), "n must be a non-negative integer of length 1")
})

test_that("nth consumes an iterator when n < length(iterable)", {
  it <- iteror(letters)
  expect_equal(nth(it, 5, NA), "e")
})

test_that("nth returns the given default value when the iterator is consumed", {
  it <- iteror(letters)
  expect_equal(nth(it, 27, NA), NA)
})

test_that("nth rejects negative or non-integer n", {
  it <- iteror(letters)
  expect_error(nth(it, -1, NA), "n must be a positive integer of length 1")
  expect_error(nth(it, "a", NA), "n must be a positive integer of length 1")
})
