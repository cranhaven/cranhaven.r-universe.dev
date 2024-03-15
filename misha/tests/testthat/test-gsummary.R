test_that("gsummary with test.fixedbin", {
    expect_regression(gsummary("test.fixedbin"), "gsummary_fixedbin")
})

test_that("gsummary with test.sparse", {
    expect_regression(gsummary("test.sparse"), "gsummary_sparse")
})

test_that("gsummary with test.array", {
    expect_regression(gsummary("test.array"), "gsummary_array")
})

test_that("gsummary with test.rects", {
    expect_regression(gsummary("test.rects"), "gsummary_rects")
})

test_that("gsummary with test.computed2d", {
    expect_regression(gsummary("test.computed2d"), "gsummary_computed2d")
})

test_that("gsummary with test.fixedbin after gscreen filtering", {
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2), 0, -1))
    expect_regression(gsummary("test.fixedbin", intervs), "gsummary_fixedbin_gscreen_filtered")
})

test_that("gsummary with test.sparse after gscreen filtering", {
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2), 0, -1))
    expect_regression(gsummary("test.sparse", intervs), "gsummary_sparse_gscreen_filtered")
})

test_that("gsummary with test.array after gscreen filtering", {
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2), 0, -1))
    expect_regression(gsummary("test.array", intervs), "gsummary_array_gscreen_filtered")
})

test_that("gsummary with test.rects after gscreen filtering on rects", {
    intervs <- gscreen("test.rects > 40", gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4)))
    expect_regression(gsummary("test.rects", intervs), "gsummary_rects_gscreen_filtered")
})

test_that("gsummary with test.computed2d after gscreen filtering on computed2d", {
    intervs <- gscreen("test.computed2d > 4000000", gintervals.2d(chroms1 = c(6, 5), chroms2 = c(8, 9)))
    expect_regression(gsummary("test.computed2d", intervs), "gsummary_computed2d_gscreen_filtered")
})

test_that("gsummary with test.generated_1d_1 and test.generated_1d_2 under limited data size", {
    withr::with_options(c(gmax.data.size = 150), {
        expect_regression(gsummary("test.generated_1d_1", "test.generated_1d_2"), "gsummary_generated_1d_limited_data_size")
    })
})

test_that("gsummary with test.generated_2d_6 and test.generated_2d_5 under expanded data size", {
    withr::with_options(c(gmax.data.size = 15000), {
        expect_error(gsummary("test.generated_2d_6", "test.generated_2d_5"))
    })
})
