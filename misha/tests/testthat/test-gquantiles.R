test_that("gquantiles with test.fixedbin", {
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2), 0, -1))
    result <- gquantiles("test.fixedbin+0.2", percentile = c(0.5, 0.3, 0.2, 0.9), intervs)
    expect_regression(result, "gquantiles_fixedbin_result")
})

test_that("gquantiles with test.rects", {
    result <- gquantiles("test.rects", percentile = c(0.5, 0.3, 0.2, 0.9, 0.999), gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4)))
    expect_regression(result, "gquantiles_rects_result")
})

test_that("gquantiles with test.computed2d", {
    result <- gquantiles("test.computed2d", percentile = c(0.5, 0.3, 0.2, 0.9, 0.999), gintervals.2d(chroms1 = c(6, 5), chroms2 = c(8, 9)))
    expect_regression(result, "gquantiles_computed2d_result")
})

test_that("gquantiles with test.fixedbin without intervals", {
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2), 0, -1))
    result <- gquantiles("test.fixedbin+0.2", percentile = c(0.5, 0.999))
    expect_regression(result, "gquantiles_fixedbin_no_intervals_result")
})
