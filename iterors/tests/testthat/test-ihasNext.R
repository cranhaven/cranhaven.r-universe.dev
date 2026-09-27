test_that("test01", {
    x <- rnorm(100)
    it <- ihasNext(x)
    actual <- unlist(as.list(it))
    expect_equal(x, actual)
})

test_that("test02", {
    x <- 1:10
    it <- ihasNext(x)
    i <- 0
    actual <- integer(0)
    while (hasNext(it)) {
        i <- i + 1
        actual[i] <- nextOr(it)
    }
    expect_true(i == length(x))
    expect_equal(x, actual)
})
