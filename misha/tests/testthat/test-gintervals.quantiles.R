test_that("gintervals.quantiles works", {
    expect_regression(gintervals.quantiles("test.fixedbin+0.2", percentile = c(0.5, 0.3, 0.2, 0.9), .misha$ALLGENOME), "gintervals.quantiles.1")

    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2), 0, -1))
    expect_regression(gintervals.quantiles("test.fixedbin+0.2", percentile = c(0.5, 0.3, 0.2, 0.9), intervs), "gintervals.quantiles.2")

    intervs1 <- gscreen("test.fixedbin > 0.2 & test.fixedbin < 0.3", gintervals(c(1, 2, 3), 0, -1))
    intervs2 <- gscreen("test.fixedbin > 0.25 & test.fixedbin < 0.35", gintervals(c(1, 2), 0, -1))
    set.seed(60427)
    intervs2 <- intervs2[sample(nrow(intervs2)), ]
    expect_regression(gintervals.quantiles("test.fixedbin", c(0.5, 0.2, 0.9), intervals = intervs2, iterator = intervs1), "gintervals.quantiles.3")
})

test_that("gintervals.quantiles works with intervals.set.out", {
    gintervals.rm("test.testintervs_quantiles", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs_quantiles", force = TRUE))
    intervs1 <- gscreen("test.fixedbin > 0.2 & test.fixedbin < 0.3", gintervals(c(1, 2, 3), 0, -1))
    intervs2 <- gscreen("test.fixedbin > 0.25 & test.fixedbin < 0.35", gintervals(c(1, 2), 0, -1))
    set.seed(60427)
    intervs2 <- intervs2[sample(nrow(intervs2)), ]
    gintervals.quantiles("test.fixedbin", c(0.5, 0.2, 0.9), intervals = intervs2, iterator = intervs1, intervals.set.out = "test.testintervs_quantiles")
    expect_equal(
        gintervals.load("test.testintervs_quantiles"),
        gintervals.quantiles("test.fixedbin", c(0.5, 0.2, 0.9), intervals = intervs2, iterator = intervs1)
    )
})

test_that("gintervals.quantiles works with different types of iterators", {
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 3), 0, -1))
    expect_regression(gintervals.quantiles("test.fixedbin+0.2", percentile = c(0.5, 0.3, 0.2, 0.9), intervs, iterator = "test.sparse"), "gintervals.quantiles.4")

    intervs <- gscreen("test.rects > 40", gintervals.2d(c(1, 2, 5, 8), 0, -1))
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]
    expect_error(gintervals.quantiles("test.rects", percentile = c(0.5, 0.3, 0.2, 0.9), intervs))
})

test_that("gintervals.quantiles works with intervals.set.out (2d)", {
    gintervals.rm("test.testintervs_quantiles", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs_quantiles", force = TRUE))
    intervs <- gscreen("test.rects > 40", gintervals.2d(c(1, 2, 5, 8), 0, -1))
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]
    gintervals.quantiles("test.rects", percentile = c(0.5, 0.3, 0.2, 0.9), intervs, iterator = c(1, 1), intervals.set.out = "test.testintervs_quantiles")
    expect_equal(
        gintervals.load("test.testintervs_quantiles"),
        gintervals.quantiles("test.rects", percentile = c(0.5, 0.3, 0.2, 0.9), intervs, iterator = c(1, 1))
    )
})
