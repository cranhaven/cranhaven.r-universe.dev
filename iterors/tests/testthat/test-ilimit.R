test_that("test01", {
    actual <- as.list(i_limit(icount(), 10))
    expected <- as.list(icount(10))
    expect_equal(expected, actual)
})

test_that("test02", {
    it <- icount()
    actual <- unlist(as.list(i_limit(it, 3)))
    expected <- 1:3
    expect_equal(expected, actual)
    actual <- unlist(as.list(i_limit(it, 3)))
    expected <- 4:6
    expect_equal(expected, actual)
})
