test_that("take properly extracts n elements, n < length of the iterable", {
  it <- i_chain(1:3, 4:5, 6)
  expect_equal(take(it, 4), as.list(1:4))
  expect_equal(nextOr(it, NA), 5)
  expect_equal(nextOr(it, NA), 6)
  expect_equal(nextOr(it, NA), NA)
})

test_that("take properly extracts n elements, n = length of the iterable", {
  it <- i_chain(1:3, 4:5, 6)
  expect_equal(take(it, 6), as.list(1:6))
  expect_equal(nextOr(it, NA), NA)
})

test_that("take properly extracts n elements, n > length of the iterable", {
  it <- i_chain(1:3, 4:5, 6)
  expect_equal(take(it, 7), as.list(1:6))
  expect_equal(nextOr(it, NA), NA)
})
