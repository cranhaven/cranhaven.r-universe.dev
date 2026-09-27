test_that("test isplit with a single factor", {
  x <- rnorm(200)
  f <- factor(sample(1:10, length(x), replace=TRUE))

  it <- isplit(x, f)
  expected <- split(x, f)

  for (i in expected) {
    actual <- nextOr(it)
    expect_equal(actual$value, i)
  }

  it <- isplit(x, f, drop=TRUE)
  expected <- split(x, f, drop=TRUE)

  for (i in expected) {
    actual <- nextOr(it)
    expect_equal(actual$value, i)
  }
})

test_that("test isplit with two factors", {
  x <- rnorm(200)
  f <- list(factor(sample(1:10, length(x), replace=TRUE)),
            factor(sample(1:10, length(x), replace=TRUE)))

  it <- isplit(x, f)
  expected <- split(x, f)

  for (i in expected) {
    actual <- nextOr(it)
    expect_equal(actual$value, i)
  }

  it <- isplit(x, f, drop=TRUE)
  expected <- split(x, f, drop=TRUE)

  for (i in expected) {
    actual <- nextOr(it)
    expect_equal(actual$value, i)
  }
})
