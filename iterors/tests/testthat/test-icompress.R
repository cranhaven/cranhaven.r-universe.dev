test_that("Apply i_mask to an integer sequence", {
  n <- 10
  selectors <- rep(c(F, T), n)
  it <- i_mask(1:10, selectors)

  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), 6)
  expect_equal(nextOr(it, NA), 8)
  expect_equal(nextOr(it, NA), 10)
  expect_equal(nextOr(it, NA), NA)
})

test_that("Apply i_mask to an integer sequence using anonymous function", {
  n <- 10
  it <- i_mask(1:10, rep(c(T, F), n))

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 5)
  expect_equal(nextOr(it, NA), 7)
  expect_equal(nextOr(it, NA), 9)
  expect_equal(nextOr(it, NA), NA)
})

test_that("Apply i_mask to a character vector", {
  it <- i_mask(letters, letters %in% c('a', 'e', 'i', 'o', 'u'))

  expect_equal(nextOr(it, NA), 'a')
  expect_equal(nextOr(it, NA), 'e')
  expect_equal(nextOr(it, NA), 'i')
  expect_equal(nextOr(it, NA), 'o')
  expect_equal(nextOr(it, NA), 'u')
  expect_equal(nextOr(it, NA), NA)
})
