test_that("Apply i_dropwhile to an integer sequence", {

  not_too_large <- function(x) {
    x <= 3
  }
  it <- i_dropwhile(1:10, not_too_large)

  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), 5)
  expect_equal(nextOr(it, NA), 6)
  expect_equal(nextOr(it, NA), 7)
  expect_equal(nextOr(it, NA), 8)
  expect_equal(nextOr(it, NA), 9)
  expect_equal(nextOr(it, NA), 10)
  expect_equal(nextOr(it, NA), NA)
})

test_that("Apply i_dropwhile to an integer sequence using anonymous function", {
  it <- i_dropwhile(seq(2, 20, by=2), function(x) x <= 10)

  expect_equal(nextOr(it, NA), 12)
  expect_equal(nextOr(it, NA), 14)
  expect_equal(nextOr(it, NA), 16)
  expect_equal(nextOr(it, NA), 18)
  expect_equal(nextOr(it, NA), 20)
  expect_equal(nextOr(it, NA), NA)
})
