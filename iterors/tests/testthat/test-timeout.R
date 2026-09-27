test_that("test01", {
    it <- ihasNext(i_timeout(icount(), 2))
    x <- 0
    while (hasNext(it)) x <- nextOr(it, break)
    expect_true(x > 0)
})

test_that("test02", {
    n <- 1000
    actual <- as.list(i_timeout(icount(n), time = Inf))
    expected <- as.list(1:n)
    expect_equal(expected, actual)
})
