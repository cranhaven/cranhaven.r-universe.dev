library(testthat)
library(whSample)

test_that(
  "Checking ssize",
  {
    expect_equal(ssize(10000), 193)
    expect_equal(ssize(10000, ci=0.8), 84)
    expect_equal(ssize(10000, me=0.06), 260)
    expect_equal(ssize(10000, p=0.60), 185)

  }
)

