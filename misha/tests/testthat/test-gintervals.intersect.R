test_that("gintervals.intersect works (1)", {
    intervs1 <- gscreen("test.fixedbin > 0.1", gintervals(c(1, 2), 0, -1))
    intervs2 <- gscreen("test.fixedbin < 0.2", gintervals(c(1, 2), 0, -1))
    expect_regression(gintervals.intersect(intervs1, intervs2), "gintervals.intersect.1")
})

test_that("gintervals.intersect works (2)", {
    intervs1 <- gscreen("test.fixedbin > 0.1 & test.fixedbin < 0.3", gintervals(c(1, 2)))
    intervs2 <- gscreen("test.fixedbin > 0.2 & test.fixedbin < 0.4", gintervals(c(1, 2)))
    intervs3 <- gscreen("test.fixedbin > 0.34 & test.fixedbin < 0.5", gintervals(c(1, 2)))
    expect_regression(gintervals.intersect(rbind(intervs1, intervs2), intervs3), "gintervals.intersect.2")
})

test_that("gintervals.intersect works (3)", {
    intervs1 <- gscreen("test.rects > 40", gintervals.2d(c(1, 2, 5, 8), 0, -1))
    intervs2 <- gscreen("test.rects < 60", gintervals.2d(c(2, 4, 5, 9), 0, -1))
    expect_regression(gintervals.intersect(intervs1, intervs2), "gintervals.intersect.3")
})


test_that("gintervals.intersect works on named intervals", {
    expect_regression(gintervals.intersect("test.bigintervs_1d_1", "test.bigintervs_1d_2"), "gintervals.intersect.named.1")
    expect_regression(gintervals.intersect("test.generated_1d_1", "test.generated_1d_2"), "gintervals.intersect.named.2")
    expect_regression(gintervals.intersect("test.bigintervs_2d_5", "test.bigintervs_2d_6"), "gintervals.intersect.named.3")
    expect_regression(gintervals.intersect("test.generated_2d_5", "test.generated_2d_6"), "gintervals.intersect.named.4")
})

test_that("cannot intersect 1d with 2d", {
    expect_error(gintervals.intersect(gintervals(1), gintervals.2d(1)))
})

test_that("gintervals intersect with intervals.set.out", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    intervs1 <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2, 4, 8, 9), 0, -1))
    intervs2 <- gscreen("test.fixedbin > 0.4", gintervals(c(1, 2, 4, 7, 9), 0, -1))
    gintervals.intersect(intervs1, intervs2, intervals.set.out = "test.testintervs")
    expect_equal(gintervals.load("test.testintervs"), gintervals.intersect(intervs1, intervs2))
})

test_that("cannot diff 2d", {
    expect_error(gintervals.diff(gintervals.2d(5), gintervals.2d(1)))
})

test_that("gintervals.union works (1)", {
    intervs1 <- gscreen("test.fixedbin > 0.1 & test.fixedbin < 0.3", gintervals(c(1, 2), 0, -1))
    intervs2 <- gscreen("test.fixedbin < 0.2", gintervals(c(1, 2), 0, -1))
    expect_regression(gintervals.union(intervs1, intervs2), "gintervals.union.1")
})

test_that("gintervals.union works (2)", {
    intervs1 <- gscreen("test.fixedbin > 0.1 & test.fixedbin < 0.3", gintervals(c(1, 2)))
    intervs2 <- gscreen("test.fixedbin > 0.2 & test.fixedbin < 0.4", gintervals(c(1, 2)))
    intervs3 <- gscreen("test.fixedbin > 0.34 & test.fixedbin < 0.5", gintervals(c(1, 2)))
    expect_regression(gintervals.union(rbind(intervs1, intervs2), intervs3), "gintervals.union.2")
})

test_that("cannot union 2d", {
    intervs1 <- gscreen("test.rects > 40 & test.rects < 80", gintervals.2d(c(1, 2, 5, 8), 0, -1))
    intervs2 <- gscreen("test.rects < 60", gintervals.2d(c(2, 4, 5, 9), 0, -1))
    expect_error(expect_regression(gintervals.union(intervs1, intervs2)))
    expect_error(gintervals.union(gintervals(1), gintervals.2d(1)))
})

test_that("gintervals.union works (3)", {
    expect_regression(gintervals.union("test.bigintervs_1d_1", "test.bigintervs_1d_2"), "gintervals.union.3")
})
