remove_all_vtracks <- function() {
    vtracks <- gvtrack.ls()
    for (vtrack in vtracks) {
        do.call(gvtrack.rm, list(vtrack = vtrack))
    }
}

test_that("vtrack.sparse extraction without function", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = "test.sparse")
    expect_regression(r, "vtrack.sparse.basic")
})

test_that("vtrack.array extraction without function", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = "test.array")
    expect_regression(r, "vtrack.array.basic")
})

test_that("vtrack.sparse extraction with blabla function", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.fixedbin", func = "blabla"))
})

test_that("vtrack.array extraction with blabla function", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.fixedbin", func = "blabla"))
})

test_that("vtrack.sparse extraction with avg function and numeric iterator", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.fixedbin", func = "avg", 10))
})

test_that("vtrack.sparse extraction with avg function", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin", func = "avg")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 233)
    expect_regression(r, "vtrack.sparse.avg")
})

test_that("vtrack.sparse extraction with avg function and high iterator", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.sparse", func = "avg")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 10000)
    expect_regression(r, "vtrack.sparse.avg.high")
})

test_that("vtrack.array extraction with avg function and high iterator", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array", func = "avg")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 10000)
    expect_regression(r, "vtrack.array.avg.high")
})

test_that("vtrack.rects extraction with avg function and 2d iterator", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.rects", func = "avg")
    r <- gextract("v1", gintervals.2d(chroms1 = c(1, 3), 3000000, -1, chroms2 = c(1, 4), 3000000, -1), iterator = c(2000000, 3000000))
    expect_regression(r, "vtrack.rects.avg.2d")
})

test_that("vtrack.computed2d extraction with avg function and 2d iterator", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.computed2d", func = "avg")
    r <- gextract("v1", gintervals.2d(chroms1 = c(6, 1, 5), 3000000, -1, chroms2 = c(8, 1, 9), 3000000, -1), iterator = c(2000000, 3000000))
    expect_regression(r, "vtrack.computed2d.avg.2d")
})

test_that("vtrack.sparse extraction with max function and numeric iterator", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.fixedbin", func = "max", 10))
})

test_that("vtrack.sparse extraction with max function", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin", func = "max")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 233)
    expect_regression(r, "vtrack.sparse.max")
})

test_that("vtrack.sparse extraction with max function and high iterator", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.sparse", func = "max")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 10000)
    expect_regression(r, "vtrack.sparse.max.high")
})

test_that("vtrack.array extraction with max function and high iterator", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array", func = "max")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 10000)
    expect_regression(r, "vtrack.array.max.high")
})

test_that("vtrack.rects extraction with max function and 2d iterator", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.rects", func = "max")
    r <- gextract("v1", gintervals.2d(chroms1 = c(1, 3), 3000000, -1, chroms2 = c(1, 4), 3000000, -1), iterator = c(2000000, 3000000))
    expect_regression(r, "vtrack.rects.max.2d")
})

test_that("vtrack.computed2d extraction with max function and 2d iterator", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.computed2d", func = "max")
    r <- gextract("v1", gintervals.2d(chroms1 = c(6, 1, 5), 3000000, -1, chroms2 = c(8, 1, 9), 3000000, -1), iterator = c(2000000, 3000000))
    expect_regression(r, "vtrack.computed2d.max.2d")
})

test_that("fixedbin_min_10", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.fixedbin", func = "min", 10))
})

test_that("fixedbin_min", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin", func = "min")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 233)
    expect_regression(r, "vtrack.fixedbin_min")
})

test_that("sparse_min", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.sparse", func = "min")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 10000)
    expect_regression(r, "vtrack.sparse_min")
})

test_that("array_min", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array", func = "min")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 10000)
    expect_regression(r, "vtrack.array_min")
})

test_that("rects_min", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.rects", func = "min")
    r <- gextract("v1", gintervals.2d(chroms1 = c(1, 3), 3000000, -1, chroms2 = c(1, 4), 3000000, -1), iterator = c(2000000, 3000000))
    expect_regression(r, "vtrack.rects_min")
})

test_that("computed2d_min", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.computed2d", func = "min")
    r <- gextract("v1", gintervals.2d(chroms1 = c(6, 1, 5), 3000000, -1, chroms2 = c(8, 1, 9), 3000000, -1), iterator = c(2000000, 3000000))
    expect_regression(r, "vtrack.computed2d_min")
})

test_that("fixedbin_nearest_10", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.fixedbin", func = "nearest", 10))
})

test_that("fixedbin_nearest", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin", func = "nearest")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 233)
    expect_regression(r, "vtrack.fixedbin_nearest")
})

test_that("sparse_nearest", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.sparse", func = "nearest")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 10000)
    expect_regression(r, "vtrack.sparse_nearest")
})

test_that("array_nearest", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array", func = "nearest")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 10000)
    expect_regression(r, "vtrack.array_nearest")
})

test_that("rects_nearest", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.rects", func = "nearest"))
})

test_that("computed2d_nearest", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.computed2d", func = "nearest"))
})

test_that("fixedbin_stddev_10", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.fixedbin", func = "stddev", 10))
})

test_that("fixedbin_stddev", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin", func = "stddev")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 233)
    expect_regression(r, "vtrack.fixedbin_stddev")
})

test_that("sparse_stddev", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.sparse", func = "stddev")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 10000)
    expect_regression(r, "vtrack.sparse_stddev")
})

test_that("array_stddev", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array", func = "stddev")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 10000)
    expect_regression(r, "vtrack.array_stddev")
})

test_that("rects_stddev", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.rects", func = "stddev"))
})

test_that("computed2d_stddev", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.computed2d", func = "stddev"))
})

test_that("fixedbin_sum_with_params", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.fixedbin", func = "sum", 10))
})

test_that("fixedbin_sum", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin", func = "sum")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 233)
    expect_regression(r, "vtrack.fixedbin_sum")
})

test_that("sparse_sum", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.sparse", func = "sum")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 10000)
    expect_regression(r, "vtrack.sparse_sum")
})

test_that("array_sum", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array", func = "sum")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 10000)
    expect_regression(r, "vtrack.array_sum")
})

test_that("rects_sum", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.rects", func = "sum"))
})

test_that("computed2d_sum", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.computed2d", func = "sum"))
})

test_that("fixed_bin_quantile", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.fixedbin", func = "quantile"))
})

test_that("fixed_bin_quantile", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin", func = "quantile", params = 0.5)
    gvtrack.create("v2", "test.fixedbin", func = "quantile", params = 0.9)
    r <- gextract("v1", "v2", gintervals(c(1, 2)), iterator = 233)
    expect_regression(r, "vtrack.fixedbin_quantile")
})

test_that("vtrack.fixedbin quantile extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin", func = "quantile", params = 0.5)
    gvtrack.create("v2", "test.fixedbin", func = "quantile", params = 0.9)
    r <- gextract("v1", "v2", gintervals(c(1, 2)), iterator = 10000)
    expect_regression(r, "vtrack.fixedbin.quantile_extraction_1")
})

test_that("vtrack.fixedbin quantile extraction 2D chroms1=1,3; chroms2=1,4", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin", func = "quantile", params = 0.5)
    gvtrack.create("v2", "test.fixedbin", func = "quantile", params = 0.9)
    expect_error(gextract("v1", "v2", gintervals.2d(chroms1 = c(1, 3), 3000000, -1, chroms2 = c(1, 4), 3000000, -1), iterator = c(2000000, 3000000)))
})

test_that("vtrack.fixedbin quantile extraction 2D chroms1=6,1,5; chroms2=8,1,9", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin", func = "quantile", params = 0.5)
    gvtrack.create("v2", "test.fixedbin", func = "quantile", params = 0.9)
    expect_error(gextract("v1", "v2", gintervals.2d(chroms1 = c(6, 1, 5), 3000000, -1, chroms2 = c(8, 1, 9), 3000000, -1), iterator = c(2000000, 3000000)))
})

test_that("vtrack.fixedbin global percentile with param 10 extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.fixedbin", func = "global.percentile", params = 10))
})

test_that("vtrack.fixedbin global percentile extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin", func = "global.percentile")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 233)
    expect_regression(r, "vtrack.fixedbin.global_percentile_extraction")
})

test_that("vtrack.sparse global percentile extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.sparse", func = "global.percentile"))
})

test_that("vtrack.array global percentile extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.array", func = "global.percentile"))
})

test_that("vtrack.rects global percentile extraction 2D chroms1=1,3; chroms2=1,4", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.rects", func = "global.percentile"))
})

test_that("vtrack.computed2d global percentile extraction 2D chroms1=6,1,5; chroms2=8,1,9", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.computed2d", func = "global.percentile"))
})

test_that("vtrack.fixedbin global percentile max with param 10 extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.fixedbin", func = "global.percentile.max", params = 10))
})

test_that("vtrack.fixedbin global percentile max extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin", func = "global.percentile.max")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 233)
    expect_regression(r, "vtrack.fixedbin.global_percentile_max_extraction")
})

test_that("vtrack.sparse global percentile max extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.sparse", func = "global.percentile.max"))
})

test_that("vtrack.array global percentile max extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.array", func = "global.percentile.max"))
})

test_that("vtrack.rects global percentile max extraction 2D chroms1=1,3; chroms2=1,4", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.rects", func = "global.percentile.max"))
})

test_that("vtrack.computed2d global percentile max extraction 2D chroms1=6,1,5; chroms2=8,1,9", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.computed2d", func = "global.percentile.max"))
})

test_that("vtrack.fixedbin global percentile min with param 10 extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.fixedbin", func = "global.percentile.min", params = 10))
})

test_that("vtrack.fixedbin extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin", func = "global.percentile.min")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 233)
    expect_regression(r, "vtrack.fixedbin.result")
})

test_that("vtrack.sparse extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.sparse", func = "global.percentile.min"))
})

test_that("vtrack.array extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.array", func = "global.percentile.min"))
})

test_that("vtrack.rects extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.rects", func = "global.percentile.min"))
})

test_that("vtrack.computed2d extraction", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_error(gvtrack.create("v1", "test.computed2d", func = "global.percentile.min"))
})

test_that("vtrack.fixedbin with gscreen and blabla", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    intervs <- gscreen("test.fixedbin > 0.5", gintervals(c(1, 3), 0, -1))
    expect_error(gvtrack.create("v1", intervs, "blabla"))
})

test_that("vtrack.fixedbin with gscreen", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    intervs <- gscreen("test.fixedbin > 0.5", gintervals(c(1, 3), 0, -1))
    gvtrack.create("v1", intervs)
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 533)
    expect_regression(r, "vtrack.fixedbin.gscreen.result")
})

test_that("vtrack.fixedbin with gscreen and positive strand", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    intervs <- gscreen("test.fixedbin > 0.5", gintervals(c(1, 3), 0, -1))
    intervs$strand <- 1
    gvtrack.create("v1", intervs, "distance")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 533)
    expect_regression(r, "vtrack.fixedbin.positive.result")
})

test_that("vtrack.fixedbin with gscreen and negative strand", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    intervs <- gscreen("test.fixedbin > 0.5", gintervals(c(1, 3), 0, -1))
    intervs$strand <- -1
    gvtrack.create("v1", intervs, "distance")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 533)
    expect_regression(r, "vtrack.fixedbin.negative.result")
})

test_that("vtrack.gextract_with_fixedbin_strand1_distance_center_200", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    intervs <- gscreen("test.fixedbin > 0.5", gintervals(c(1, 3), 0, -1))
    intervs$strand <- 1
    expect_error(gvtrack.create("v1", intervs, "distance.center", 200))
})

test_that("vtrack.gextract_with_fixedbin_strand_negative1_distance_center_200", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    intervs <- gscreen("test.fixedbin > 0.5", gintervals(c(1, 3), 0, -1))
    intervs$strand <- -1
    expect_error(gvtrack.create("v1", intervs, "distance.center", 200))
})

test_that("vtrack.gextract_with_fixedbin_strand1_distance_center", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    intervs <- gscreen("test.fixedbin > 0.5", gintervals(c(1, 3), 0, -1))
    intervs$strand <- 1
    expect_error(gvtrack.create("v1", intervs, "distance.center", 100))
})

test_that("vtrack.gextract_with_fixedbin_distance_center", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    intervs <- gscreen("test.fixedbin > 0.5", gintervals(c(1, 3), 0, -1))
    gvtrack.create("v1", intervs, "distance.center")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 533)
    expect_regression(r, "vtrack.reg_fixedbin_distance_center")
})

test_that("vtrack.gextract_with_fixedbin_strand1_distance_center_without_param", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    intervs <- gscreen("test.fixedbin > 0.5", gintervals(c(1, 3), 0, -1))
    intervs$strand <- 1
    gvtrack.create("v1", intervs, "distance.center")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 533)
    expect_regression(r, "vtrack.reg_fixedbin_strand1_distance_center_without_param")
})

# Test for iterator 533
test_that("vtrack.iterator_533", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    intervs <- gscreen("test.fixedbin > 0.5", gintervals(c(1, 3), 0, -1))
    intervs$strand <- -1
    gvtrack.create("v1", intervs, "distance.center")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = 533)
    expect_regression(r, "vtrack.iterator_533_regression")
})

# Test for iterator test.sparse
test_that("vtrack.iterator_test.sparse", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    intervs <- gscreen("test.fixedbin > 0.5", gintervals(c(1, 3), 0, -1))
    gvtrack.create("v1", intervs, "distance.center")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = "test.sparse")
    expect_regression(r, "vtrack.iterator_test.sparse_regression")
})

# Test for iterator test.array
test_that("vtrack.iterator_test.array", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    intervs <- gscreen("test.fixedbin > 0.5", gintervals(c(1, 3), 0, -1))
    gvtrack.create("v1", intervs, "distance.center")
    r <- gextract("v1", gintervals(c(1, 2)), iterator = "test.array")
    expect_regression(r, "vtrack.iterator_test.array_regression")
})

# Test for iterator test.rects
test_that("vtrack.iterator_test.rects", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    intervs <- gscreen("test.fixedbin > 0.5", gintervals(c(1, 3), 0, -1))
    gvtrack.create("v1", intervs, "distance.center")
    expect_error(gextract("v1", gintervals.2d(chroms1 = c(1, 3), 3000000, -1, chroms2 = c(1, 4), 3000000, -1), iterator = "test.rects"))
})

# Test for iterator test.computed2d
test_that("vtrack.iterator_test.computed2d", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    intervs <- gscreen("test.fixedbin > 0.5", gintervals(c(6, 1, 5), 0, -1))
    gvtrack.create("v1", intervs, "distance.center")
    expect_error(gextract("v1", gintervals.2d(chroms1 = c(6, 1, 5), 3000000, -1, chroms2 = c(8, 1, 9), 3000000, -1), iterator = "test.computed2d"))
})

# Test for track type test.fixedbin
test_that("vtrack.tracktype_test.fixedbin", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin")
    expect_error(gvtrack.array.slice("v1"))
})

# Test for track type test.sparse
test_that("vtrack.tracktype_test.sparse", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.sparse")
    expect_error(gvtrack.array.slice("v1"))
})

# Test for track type test.array
test_that("vtrack.tracktype_test.array", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    gvtrack.array.slice("v1")
    r <- gvtrack.ls()
    expect_regression(r, "vtrack.tracktype_test.array_regression")
})

# Test for track type test.rects
test_that("vtrack.tracktype_test.rects", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.rects")
    expect_error(gvtrack.array.slice("v1"))
})

# Test for track type test.computed2d
test_that("vtrack.tracktype_test.computed2d", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.computed2d")
    expect_error(gvtrack.array.slice("v1"))
})

# Test for track type test.array with slice -50
test_that("vtrack.tracktype_test.array_slice_-50", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    expect_error(expect_warning(gvtrack.array.slice("v1", -50)))
})

# Test for track type test.array with slice 50
test_that("vtrack.tracktype_test.array_slice_50", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    expect_error(expect_warning(gvtrack.array.slice("v1", 50)))
})

# Test for track type test.array with slice "blabla"
test_that("vtrack.tracktype_test.array_slice_blabla", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    expect_error(gvtrack.array.slice("v1", "blabla"))
})

# Test for track type test.array with a slice of columns
test_that("vtrack.tracktype_test.array_slice_columns", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    gvtrack.array.slice("v1", c("col1", "col1", "col3", "col5"))
    r <- gextract("v1", gintervals(c(1, 2)))
    expect_regression(r, "vtrack.tracktype_test.array_slice_columns_regression")
})

# Test for track type test.array with specific columns slice
test_that("vtrack.tracktype_test.array_slice_col135", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    gvtrack.array.slice("v1", c("col1", "col3", "col5"))
    r <- gextract("v1", gintervals(c(1, 2)))
    expect_regression(r, "vtrack.tracktype_test.array_slice_col135_regression")
})

# Test for track type test.array with columns slice and blabla parameter
test_that("vtrack.tracktype_test.array_slice_col135_blabla", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    expect_error(gvtrack.array.slice("v1", c("col1", "col3", "col5"), "blabla"))
})

# Test for track type test.array with columns slice and avg parameter
test_that("vtrack.tracktype_test.array_slice_col135_avg", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    gvtrack.array.slice("v1", c("col1", "col3", "col5"), "avg")
    r <- gextract("v1", gintervals(c(1, 2)))
    expect_regression(r, "vtrack.tracktype_test.array_slice_col135_avg_regression")
})

# Test for track type test.array with columns slice, avg parameter, and value 25
test_that("vtrack.tracktype_test.array_slice_col135_avg_25", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    expect_error(gvtrack.array.slice("v1", c("col1", "col3", "col5"), "avg", 25))
})

# Test for track type test.array with columns slice and min parameter
test_that("vtrack.tracktype_test.array_slice_col135_min", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    gvtrack.array.slice("v1", c("col1", "col3", "col5"), "min")
    r <- gextract("v1", gintervals(c(1, 2)))
    expect_regression(r, "vtrack.tracktype_test.array_slice_col135_min_regression")
})

# Test for track type test.array with columns slice and max parameter
test_that("vtrack.tracktype_test.array_slice_col135_max", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    gvtrack.array.slice("v1", c("col1", "col3", "col5"), "max")
    r <- gextract("v1", gintervals(c(1, 2)))
    expect_regression(r, "vtrack.tracktype_test.array_slice_col135_max_regression")
})

# Test for track type test.array with columns slice and stddev parameter
test_that("vtrack.tracktype_test.array_slice_col13568_stddev", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    gvtrack.array.slice("v1", c("col1", "col3", "col5", "col6", "col8"), "stddev")
    r <- gextract("v1", gintervals(c(1, 2)))
    expect_regression(r, "vtrack.tracktype_test.array_slice_col13568_stddev_regression")
})

# Test for track type test.array with columns slice and sum parameter
test_that("vtrack.tracktype_test.array_slice_col13568_sum", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    gvtrack.array.slice("v1", c("col1", "col3", "col5", "col6", "col8"), "sum")
    r <- gextract("v1", gintervals(c(1, 2)))
    expect_regression(r, "vtrack.tracktype_test.array_slice_col13568_sum_regression")
})

# Test for track type test.array with columns slice and quantile parameter
test_that("vtrack.tracktype_test.array_slice_col13568_quantile", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    expect_error(gvtrack.array.slice("v1", c("col1", "col3", "col5", "col6", "col8"), "quantile"))
})

# Test for track type test.array with columns slice, quantile parameter, and value 0.4
test_that("vtrack.tracktype_test.array_slice_col13568_quantile_0.4", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.array")
    gvtrack.array.slice("v1", c("col1", "col3", "col5", "col6", "col8"), "quantile", 0.4)
    r <- gextract("v1", gintervals(c(1, 2)))
    expect_regression(r, "vtrack.tracktype_test.array_slice_col13568_quantile_0.4_regression")
})

# Test for track type test.fixedbin with function max
test_that("vtrack.tracktype_test.fixedbin_func_max", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin", func = "max")
    r <- gvtrack.info("v1")
    expect_regression(r, "vtrack.tracktype_test.fixedbin_func_max_regression")
})

# Test for track type test.fixedbin with iterator parameters: dim 1, 830, -724
test_that("vtrack.tracktype_test.fixedbin_iterator_dim1_830_-724", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin")
    gvtrack.iterator("v1", dim = 1, 830, -724)
    expect_error(gextract("v1", gintervals(c(1, 2))))
})

# Test for track type test.fixedbin with iterator parameters: dim 0, sshift -130, eshift 224
test_that("vtrack.tracktype_test.fixedbin_iterator_dim0_sshift-130_eshift224", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin")
    gvtrack.iterator("v1", dim = 0, sshift = -130, eshift = 224)
    r <- gextract("v1", gintervals(c(1, 2)))
    expect_regression(r, "vtrack.tracktype_test.fixedbin_iterator_dim0_sshift-130_eshift224_regression")
})

# Test for track type test.fixedbin with iterator parameters: sshift -130, eshift 224
test_that("vtrack.tracktype_test.fixedbin_iterator_sshift-130_eshift224", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin")
    gvtrack.iterator("v1", sshift = -130, eshift = 224)
    r <- gextract("v1", gintervals(c(1, 2)))
    expect_regression(r, "vtrack.tracktype_test.fixedbin_iterator_sshift-130_eshift224_regression")
})

# Test for track type test.rects with iterator parameters: sshift -130, eshift 224
test_that("vtrack.tracktype_test.rects_iterator_sshift-130_eshift224", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.rects")
    expect_error(gvtrack.iterator("v1", sshift = -130, eshift = 224))
})

# Test for track type test.computed2d with iterator parameters: sshift -130, eshift 224
test_that("vtrack.tracktype_test.computed2d_iterator_sshift-130_eshift224", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.computed2d")
    expect_error(gvtrack.iterator("v1", sshift = -130, eshift = 224))
})

# Test for track type test.fixedbin with iterator parameters: dim 1 and 2D extraction
test_that("vtrack.tracktype_test.fixedbin_iterator_dim1_gintervals2d_testrects", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin")
    gvtrack.iterator("v1", dim = 1)
    r <- gextract("v1", gintervals.2d(chroms1 = c(1, 3), 3000000, -1, chroms2 = c(2, 4), 3000000, -1), iterator = "test.rects")
    expect_regression(r, "vtrack.tracktype_test.fixedbin_iterator_dim1_gintervals2d_testrects_regression")
})

# Test for track type test.fixedbin with iterator parameters: dim 2 and 2D extraction with test.rects
test_that("vtrack.fixedbin_dim2_gintervals2d_testrects", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin")
    gvtrack.iterator("v1", dim = 2)
    r <- gextract("v1", gintervals.2d(chroms1 = c(1, 3), 3000000, -1, chroms2 = c(2, 4), 3000000, -1), iterator = "test.rects")
    expect_regression(r, "vtrack.fixedbin_dim2_gintervals2d_testrects_regression")
})

# Test for track type test.fixedbin with iterator parameters: dim 1, sshift -130, eshift 224 and 2D extraction with test.rects
test_that("vtrack.fixedbin_dim1_shifts_gintervals2d_testrects", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin")
    gvtrack.iterator("v1", dim = 1, sshift = -130, eshift = 224)
    r <- gextract("v1", gintervals.2d(chroms1 = c(1, 3), 3000000, -1, chroms2 = c(2, 4), 3000000, -1), iterator = "test.rects")
    expect_regression(r, "vtrack.fixedbin_dim1_shifts_gintervals2d_testrects_regression")
})

# Test for track type test.fixedbin with iterator parameters: dim 1 and 2D extraction with test.computed2d
test_that("vtrack.fixedbin_dim1_gintervals2d_testcomputed2d_v1", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin")
    gvtrack.iterator("v1", dim = 1)
    r <- gextract("v1", gintervals.2d(chroms1 = c(6, 1, 5), 3000000, -1, chroms2 = c(8, 1, 9), 3000000, -1), iterator = "test.computed2d")
    expect_regression(r, "vtrack.fixedbin_dim1_gintervals2d_testcomputed2d_v1_regression")
})

# Test for track type test.fixedbin with iterator parameters: dim 2 and 2D extraction with test.computed2d
test_that("vtrack.fixedbin_dim2_gintervals2d_testcomputed2d_v1", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin")
    gvtrack.iterator("v1", dim = 2)
    r <- gextract("v1", gintervals.2d(chroms1 = c(6, 1, 5), 3000000, -1, chroms2 = c(8, 1, 9), 3000000, -1), iterator = "test.computed2d")
    expect_regression(r, "vtrack.fixedbin_dim2_gintervals2d_testcomputed2d_v1_regression")
})

# Test for track type test.fixedbin with iterator parameters: dim 1, sshift -130, eshift 224 and 2D extraction with test.computed2d
test_that("vtrack.fixedbin_dim1_shifts_gintervals2d_testcomputed2d_v1", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin")
    gvtrack.iterator("v1", dim = 1, sshift = -130, eshift = 224)
    r <- gextract("v1", gintervals.2d(chroms1 = c(6, 1, 5), 3000000, -1, chroms2 = c(8, 1, 9), 3000000, -1), iterator = "test.computed2d")
    expect_regression(r, "vtrack.fixedbin_dim1_shifts_gintervals2d_testcomputed2d_v1_regression")
})

# Test for track type test.fixedbin with 2D iterator
test_that("vtrack.fixedbin_iterator2d_v1", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.fixedbin")
    expect_error(gvtrack.iterator.2d("v1"))
})



# Test for track type test.rects with 2D iterator having custom shifts and 2D extraction with test.rects
test_that("vtrack.rects_iterator2d_customShifts_gintervals2d_testrects", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.rects")
    gvtrack.iterator.2d("v1", sshift1 = -1000000, eshift1 = -500000, sshift2 = 2000000, eshift2 = 2800000)
    r <- gextract("v1", gintervals.2d(chroms1 = c(1, 3), 3000000, -1, chroms2 = c(2, 4), 3000000, -1), iterator = "test.rects")
    expect_regression(r, "vtrack.rects_iterator2d_customShifts_gintervals2d_testrects_regression")
})

# Test for track type test.computed2d with 2D iterator having custom shifts and 2D extraction with test.computed2d
test_that("vtrack.computed2d_iterator2d_customShifts_gintervals2d_testcomputed2d", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.computed2d")
    gvtrack.iterator.2d("v1", sshift1 = -1000000, eshift1 = -500000, sshift2 = 2000000, eshift2 = 2800000)
    r <- gextract("v1", gintervals.2d(chroms1 = c(6, 1, 5), 3000000, -1, chroms2 = c(8, 1, 9), 3000000, -1), iterator = "test.computed2d")
    expect_regression(r, "vtrack.computed2d_iterator2d_customShifts_gintervals2d_testcomputed2d_regression")
})

# Test for multiple track creations and gvtrack.ls() functionality
test_that("vtrack.multipleCreation_gvtrackls", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.rects")
    gvtrack.create("v2", "test.sparse")
    gvtrack.create("v3", "test.computed2d")
    r <- gvtrack.ls()
    expect_regression(r, "vtrack.multipleCreation_gvtrackls_regression")
})

# Test for track creation of type test.sparse and removal of v1 track with gvtrack.ls() functionality check
test_that("vtrack.sparseCreation_v1Removal_gvtrackls", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v2", "test.sparse")
    r <- gvtrack.ls()
    expect_regression(r, "vtrack.sparseCreation_v1Removal_gvtrackls_regression")
})

# Test for track creations of types test.rects and test.sparse, checking gvtrack.ls() before and after removal of v1 track
test_that("vtrack.multipleCreation_v1Removal_gvtracklsBeforeAfter", {
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    gvtrack.create("v1", "test.rects")
    gvtrack.create("v2", "test.sparse")
    r1 <- gvtrack.ls()
    gvtrack.rm("v1")
    r2 <- gvtrack.ls()
    remove_all_vtracks()
    withr::defer(remove_all_vtracks())
    expect_equal(list(r1, r2), list(c("v1", "v2"), "v2"))
})
