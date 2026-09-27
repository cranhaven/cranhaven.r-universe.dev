test_that("Indefinite i_recycle of integer sequence", {
  it <- i_recycle(1:3)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), 1)
})

test_that("i_recycle repeats integer fixed number of times", {
  it <- i_recycle(1:3, times=2)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), 1)
  expect_equal(nextOr(it, NA), 2)
  expect_equal(nextOr(it, NA), 3)

  expect_equal(nextOr(it, NA), NA)
})

