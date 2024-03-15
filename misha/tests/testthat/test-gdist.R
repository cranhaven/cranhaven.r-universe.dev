test_that("gdist works", {
    expect_regression(gdist("test.fixedbin", seq(0, 1, by = 0.001)), "gdist.1")
    expect_regression(gdist("test.fixedbin", seq(0.2, 1, by = 0.001)), "gdist.2")
    expect_regression(gdist("test.fixedbin", seq(0, 1, by = 0.01), "test.fixedbin+0.3", seq(0, 1, by = 0.01)), "gdist.3")
})

test_that("gscreen + gdist works (1)", {
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2), 0, -1))
    expect_regression(gdist("test.fixedbin", seq(0.2, 1, by = 0.001), intervs), "gdist_gscreen.1")
})

test_that("gscreen + gdist 2d works (1)", {
    intervs <- gscreen("test.rects > 9")
    expect_regression(gdist("test.rects", seq(8, 10, by = 0.1), intervs), "gdist_gscreen_2d.1")
})

test_that("gscreen + gdist 2d works (2)", {
    intervs <- gscreen("test.computed2d > 9500000")
    expect_regression(gdist("test.computed2d", seq(9000000, 10000000, by = 100000), intervs), "gdist_gscreen_2d.2")
})
