test_that("gsample with test.sparse", {
    set.seed(60427)
    expect_regression(gsample("test.sparse", 100, gintervals(c(1, 2))), "gsample.sparse")
})

test_that("gsample with test.rects", {
    set.seed(60427)
    expect_regression(gsample("test.rects", 100, gintervals.2d(c(1, 2))), "gsample.rects")
})
