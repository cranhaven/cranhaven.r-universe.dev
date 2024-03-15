test_that("gpartition with test.fixedbin and sampling", {
    intervs <- gscreen("test.fixedbin > 0.14", gintervals(c(1, 2), 0, -1))
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]
    result <- gpartition("test.fixedbin", seq(0, 1, by = 0.1), intervals = intervs)
    expect_regression(result, "gpartition_fixedbin_sampling_result")
})

test_that("gpartition with test.rects", {
    result <- gpartition("test.rects", seq(50, 100, by = 1), intervals = gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4)))
    expect_regression(result, "gpartition_rects_result")
})

test_that("gpartition with test.computed2d", {
    result <- gpartition("test.computed2d", seq(5000000, 10000000, by = 1000000), intervals = gintervals.2d(chroms1 = c(6, 5), chroms2 = c(8, 9)))
    expect_regression(result, "gpartition_computed2d_result")
})

test_that("gpartition with test.fixedbin, sampling, and data size option", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    intervs <- gscreen("test.fixedbin > 0.14", gintervals(c(1, 2), 0, -1))
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]
    withr::with_options(c(gmax.data.size = 2000000), {
        gpartition("test.fixedbin", seq(0, 1, by = 0.1), intervals = intervs, intervals.set.out = "test.testintervs")
    })
    r <- gintervals.load("test.testintervs")
    expect_regression(r, "gpartition_fixedbin_sampling_data_size_result")
})

test_that("gpartition with test.rects and data size option", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    withr::with_options(c(gmax.data.size = 18000), {
        gpartition("test.rects", seq(0, 100, by = 1), gintervals.2d(chroms1 = c(6, 3), chroms2 = c(2, 4)), intervals.set.out = "test.testintervs")
    })
    r <- gintervals.load("test.testintervs")
    expect_regression(r, "gpartition_rects_data_size_result")
})
