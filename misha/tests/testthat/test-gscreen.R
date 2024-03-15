test_that("gscreen with test.fixedbin", {
    expect_error(gscreen("test.fixedbin"))
})

test_that("gscreen with test.rects", {
    expect_error(gscreen("test.rects"))
})

test_that("gscreen with operation on test.fixedbin (1)", {
    expect_regression(gscreen("2 * test.fixedbin+0.2 > 0.4"), "gscreen.fixedbin.1")
})

test_that("gscreen with operation on test.rects (1)", {
    expect_regression(gscreen("2 * test.rects+1 > 100"), "gscreen.rects.1")
})

test_that("gscreen with negative condition on test.fixedbin", {
    expect_regression(gscreen("test.fixedbin < -1"), "gscreen.fixedbin.2")
})

test_that("gscreen with negative condition on test.rects", {
    expect_regression(gscreen("test.rects < -1"), "gscreen.rects.2")
})

test_that("gscreen with specific range on test.fixedbin", {
    expect_regression(gscreen("test.fixedbin > 0.2", gintervals(1, c(0, 2000000, 4000000), c(1000000, 3000000, 5000000))), "gscreen.fixedbin.3")
})

test_that("gscreen with specific range on test.rects", {
    expect_regression(gscreen("test.rects > 40", gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4))), "gscreen.rects.3")
})
