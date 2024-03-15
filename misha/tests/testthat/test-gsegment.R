test_that("gsegment with test.fixedbin", {
    expect_regression(gsegment("test.fixedbin", 10000, maxpval = 0.000001), "gsegment_fixedbin")
})

test_that("gsegment with test.sparse", {
    expect_error(gsegment("test.sparse", 10000, maxpval = 0.000001))
})

test_that("gsegment with test.array", {
    expect_error(gsegment("test.array", 10000, maxpval = 0.000001))
})

test_that("gsegment with test.rects", {
    expect_error(gsegment("test.rects", 10000, maxpval = 0.000001))
})

test_that("gsegment with test.computed2d", {
    expect_error(gsegment("test.computed2d", 10000, maxpval = 0.000001))
})

test_that("gsegment with modified test.fixedbin", {
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2), 0, -1))
    expect_regression(gsegment("test.fixedbin*2", 10000, maxpval = 0.000001, intervals = intervs), "gsegment_fixedbin_mod")
})

test_that("gsegment with data size option and sampling for test.sparse", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))

    intervs <- gscreen("test.fixedbin > 0.14", gintervals(c(1, 2), 0, -1))
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]

    withr::with_options(c(gmax.data.size = 3200), {
        gsegment("test.sparse", 10000, maxpval = 0.0001, iterator = 50, intervals.set.out = "test.testintervs")
    })
    r <- gintervals.load("test.testintervs")
    expect_regression(r, "gsegment_sparse_data_size")
})
