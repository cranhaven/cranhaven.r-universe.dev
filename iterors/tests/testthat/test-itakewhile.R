test_that("Apply itakewhile to an integer sequence", {
  not_too_large <- function(x) {
    x <= 5
  }
  it <- i_keepwhile(1:100, not_too_large)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), 5)
  expect_equal(nextOr(it, NA), NA)
})

test_that("Apply itakewhile to an integer sequence using anonymous function", {
  it <- i_keepwhile(seq(2, 100, by=2), function(x) x <= 10)

  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), 6)
  expect_equal(nextOr(it, NA), 8)
  expect_equal(nextOr(it, NA), 10)
  expect_equal(nextOr(it, NA), NA)
})
