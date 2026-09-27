pow <- function(x, y) {
  x^y
}

test_that("i_starmap over a list of two numeric vectors of equal length", {
  it <- i_starmap(pow, list(c(2, 3, 10), c(5, 2, 3)))

  expect_equal(nextOr(it, NA), 32)
  expect_equal(nextOr(it, NA), 9)
  expect_equal(nextOr(it, NA), 1000)

  expect_equal(nextOr(it, NA), NA)
})

test_that("i_starmap over a list of two numeric vectors of unequal length", {
  it <- i_starmap(pow, list(c(2, 3, 10), c(5, 2, 3, 42)))

  expect_equal(nextOr(it, NA), 32)
  expect_equal(nextOr(it, NA), 9)
  expect_equal(nextOr(it, NA), 1000)

  expect_equal(nextOr(it, NA), NA)
})

test_that("i_starmap over a list of two lists of equal length", {
  it <- i_starmap(pow, list(list(2, 3, 10), list(5, 2, 3)))

  expect_equal(nextOr(it, NA), 32)
  expect_equal(nextOr(it, NA), 9)
  expect_equal(nextOr(it, NA), 1000)

  expect_equal(nextOr(it, NA), NA)
})

test_that("i_starmap over a list of two lists of unequal length", {
  it <- i_starmap(pow, list(list(2, 3, 10), list(5, 2, 3, 42)))

  expect_equal(nextOr(it, NA), 32)
  expect_equal(nextOr(it, NA), 9)
  expect_equal(nextOr(it, NA), 1000)

  expect_equal(nextOr(it, NA), NA)
})

test_that("i_starmap over the rows of a data.frame", {
  # Computes sum of each row in the iris data set
  it <- i_starmap(sum, iris[, -5])

  expect_equal(nextOr(it, NA), sum(iris[1, -5]))
  expect_equal(nextOr(it, NA), sum(iris[2, -5]))
  expect_equal(nextOr(it, NA), sum(iris[3, -5]))
  expect_equal(nextOr(it, NA), sum(iris[4, -5]))
  expect_equal(nextOr(it, NA), sum(iris[5, -5]))
  expect_equal(nextOr(it, NA), sum(iris[6, -5]))
  # and so on...
})
