test_that("iseq with default parameters is same as icount()", {
  it <- iseq()
  expect_equal(take(it, 10, "numeric"), 1:10)
  expect_equal(nextOr(it, NA), 11)
})

test_that("iseq with only from and to specified", {
  it <- iseq(from=2, to=5.5)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), 5)
  expect_equal(nextOr(it, NA), NA)
})

test_that("iseq with by specified", {
  it <- iseq(from=2, to=3.5, by=0.6)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 2.6)
  expect_equal(nextOr(it, NA), 3.2)
  expect_equal(nextOr(it, NA), NA)
})

test_that("iseq with length_out specified", {
  it <- iseq(from=2, to=3.5, length_out=5)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 2.375)
  expect_equal(nextOr(it, NA), 2.75)
  expect_equal(nextOr(it, NA), 3.125)
  expect_equal(nextOr(it, NA), 3.5)
  expect_equal(nextOr(it, NA), NA)
})

test_that("iseq with along_with specified", {
  it <- iseq(from=2, to=3.5, along_with=1:5)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 2.375)
  expect_equal(nextOr(it, NA), 2.75)
  expect_equal(nextOr(it, NA), 3.125)
  expect_equal(nextOr(it, NA), 3.5)
  expect_equal(nextOr(it, NA), NA)
})

test_that("iseq for decreasing sequence with from and to specified", {
  it <- iseq(from=2, to=-3.5)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 0)
  expect_equal(nextOr(it, NA), -1)
  expect_equal(nextOr(it, NA), -2)
  expect_equal(nextOr(it, NA), -3)
  expect_equal(nextOr(it, NA), NA)
})

test_that("iseq_len generates a finite sequence of integers", {
  it <- icount(4)
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), NA)
})

test_that("First element of iseq_len with length 0 stops", {
  it <- icount(0)
  expect_equal(nextOr(it, NA), NA)
})

test_that("iseq_along's generate a finite sequence of integers from a vector", {
  it <- icount(length(1:4))
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), NA)
})

test_that("iseq_along's generate a finite sequence of integers from a data.frame", {
  it <- icount(length(iris))
  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)
  expect_equal(nextOr(it, NA), 4)
  expect_equal(nextOr(it, NA), 5)
  expect_equal(nextOr(it, NA), NA)
})

test_that("First element of iseq_along applied to vector of length 0 yields StopIteration", {
  it <- icount(length(numeric(0)))
  expect_equal(nextOr(it, NA), NA)
})

test_that("iseq recycles", {
  it <- iseq(from=4, to=8, recycle=TRUE)
  expect_equal(take(it, 11, "numeric"), c(4:8,4:8,4))
})
