test_that("gwilcox on test.fixedbin", {
    r <- gwilcox("test.fixedbin", 100000, 1000, maxpval = 0.000001, intervals = gintervals(c(1, 2), 0, -1))
    expect_regression(r, "gwilcox.fixedbin")
})

test_that("gwilcox on test.sparse", {
    expect_error(gwilcox("test.sparse", 100000, 1000, maxpval = 0.000001, intervals = gintervals(c(1, 2), 0, -1)))
})

test_that("gwilcox on test.array", {
    expect_error(gwilcox("test.array", 100000, 1000, maxpval = 0.000001, intervals = gintervals(c(1, 2), 0, -1)))
})

test_that("gwilcox on test.rects", {
    expect_error(gwilcox("test.rects", 100000, 1000, maxpval = 0.000001, intervals = gintervals(c(1, 2), 0, -1)))
})

test_that("gwilcox on test.computed2d", {
    expect_error(gwilcox("test.computed2d", 100000, 1000, maxpval = 0.000001, intervals = gintervals(c(1, 2), 0, -1)))
})

test_that("gwilcox on test.fixedbin with screening", {
    intervs <- gscreen("test.fixedbin < 0.2", gintervals(c(1, 2), 0, -1))
    r <- gwilcox("test.fixedbin", 100000, 1000, maxpval = 0.0001, intervals = intervs)
    expect_regression(r, "gwilcox.fixedbin_screening")
})

test_that("gwilcox with interval setting and maximum data size", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    withr::with_options(c(gmax.data.size = 8700), {
        gwilcox("test.fixedbin", 100000, 1000, maxpval = 0.000001, intervals = gintervals(c(1, 2), 0, -1), intervals.set.out = "test.testintervs")
    })
    r <- gintervals.load("test.testintervs")
    expect_regression(r, "gwilcox.fixedbin_interval_setting")
})
