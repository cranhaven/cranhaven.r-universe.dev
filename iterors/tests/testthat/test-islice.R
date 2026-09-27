test_that("i_slice of integer sequence with no end specified", {
  it <- i_slice(1:5, start=2)
  expect_equal(nextOr(it, NULL), 2)
  expect_equal(nextOr(it, NULL), 3)
  expect_equal(nextOr(it, NULL), 4)
  expect_equal(nextOr(it, NULL), 5)
  expect_equal(nextOr(it, NULL), NULL)
})

test_that("i_slice of integer sequence with start and end specified", {
  it <- i_slice(1:10, start=2, end=5)
  expect_equal(nextOr(it, NULL), 2)
  expect_equal(nextOr(it, NULL), 3)
  expect_equal(nextOr(it, NULL), 4)
  expect_equal(nextOr(it, NULL), 5)
  expect_equal(nextOr(it, NULL), NULL)
})

test_that("i_slice of integer sequence with start, end, and step specified", {
  it <- i_slice(1:10, start=2, end=9, step=2)
  expect_equal(nextOr(it, NULL), 2)
  expect_equal(nextOr(it, NULL), 4)
  expect_equal(nextOr(it, NULL), 6)
  expect_equal(nextOr(it, NULL), 8)
  expect_equal(nextOr(it, NULL), NULL)
})
