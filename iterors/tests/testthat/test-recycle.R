test_that("simple test of recycle", {
  if (require(foreach, quietly=TRUE)) {
    nr <- 21
    nc <- 17
    x <- rnorm(nr)
    it <- iteror(x, recycle=TRUE)
    actual <- foreach(y=it, icount(nr*nc), .combine='c') %do% y
    dim(actual) <- c(nr, nc)
    expected <- matrix(x, nr, nc)
    expect_equal(actual, expected)
  }
})

test_that("test01", {
    for (n in c(0, 1, 2, 3, 7, 11, 13, 100, 101, 102, 103)) {
        actual <- as.list(i_recycle(icount(3)), n)
        expected <- as.list(rep(1:3, length = n))
        expect_equal(expected, actual)
    }

})

test_that("test02", {
    for (n in c(0, 1, 2, 3, 7, 11, 13, 100, 101, 102, 103)) {
        actual <- as.list(i_recycle(1:3), n)
        expected <- as.list(rep(1:3, length = n))
        expect_equal(expected, actual)
    }
})

test_that("test03", {
    expect_error(i_recycle())
})

test_that("test04", {
    expect_error(i_recycle(1:3, -1))
    expect_error(i_recycle(icount(3), -1))
})

test_that("test05", {
    x <- 1:3
    for (n in c(0, 1, 2, 3, 9)) {
        actual <- as.list(i_recycle(x, times = n))
        expected <- rep(as.list(x), times = n)
        expect_equal(expected, actual)
    }
})

test_that("test06", {
    m <- 3
    x <- seq(length = m)
    for (n in c(0, 1, 2, 3, 9)) {
        actual <- as.list(i_recycle(icount(m), times = n))
        expected <- rep(as.list(x), times = n)
        expect_equal(expected, actual)
    }
})
