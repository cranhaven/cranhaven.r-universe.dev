pow <- function(x, y) {
  x^y
}

test_that("i_map over two numeric vectors of equal length", {
  it <- i_map(pow, c(2, 3, 10), c(5, 2, 3))

  expect_equal(nextOr(it, NA), 32)
  expect_equal(nextOr(it, NA), 9)
  expect_equal(nextOr(it, NA), 1000)

  expect_equal(nextOr(it, NA), NA)
})

test_that("i_map over two numeric vectors of unequal length", {
  it <- i_map(pow, c(2, 3, 10), c(5, 2, 3, 42))

  expect_equal(nextOr(it, NA), 32)
  expect_equal(nextOr(it, NA), 9)
  expect_equal(nextOr(it, NA), 1000)

  expect_equal(nextOr(it, NA), NA)
})

test_that("i_map over two lists of equal length", {
  it <- i_map(pow, list(2, 3, 10), list(5, 2, 3))

  expect_equal(nextOr(it, NA), 32)
  expect_equal(nextOr(it, NA), 9)
  expect_equal(nextOr(it, NA), 1000)

  expect_equal(nextOr(it, NA), NA)
})

test_that("i_map over two lists of unequal length", {
  it <- i_map(pow, list(2, 3, 10), list(5, 2, 3, 42))

  expect_equal(nextOr(it, NA), 32)
  expect_equal(nextOr(it, NA), 9)
  expect_equal(nextOr(it, NA), 1000)

  expect_equal(nextOr(it, NA), NA)
})

test_that("i_map language objects", {
  it <- i_map(deparse, alist(foo, bar(baz), qux))

  expect_equal(nextOr(it, NA), "foo")
  expect_equal(nextOr(it, NA), c("bar(baz)"))
  expect_equal(nextOr(it, NA), c("qux"))

})
