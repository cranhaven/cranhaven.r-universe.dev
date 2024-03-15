test_that("gintervals.summary with test.fixedbin", {
    result <- gintervals.summary("test.fixedbin", gintervals(c(1, 2), 0, -1))
    expect_regression(result, "gintervals.summary_test.fixedbin")
})

test_that("gintervals.summary with test.sparse", {
    result <- gintervals.summary("test.sparse", gintervals(c(1, 2), 0, -1))
    expect_regression(result, "gintervals.summary_test.sparse")
})

test_that("gintervals.summary with test.rects", {
    result <- gintervals.summary("test.rects", gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4)))
    expect_regression(result, "gintervals.summary_test.rects")
})

test_that("gintervals.summary with test.computed2d", {
    result <- gintervals.summary("test.computed2d", gintervals.2d(chroms1 = c(6, 1, 5), chroms2 = c(8, 1, 9)))
    expect_regression(result, "gintervals.summary_test.computed2d")
})

test_that("gintervals.summary with randomized test.fixedbin", {
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2), 0, -1))
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]
    result <- gintervals.summary("test.fixedbin", intervs)
    expect_regression(result, "gintervals.summary_randomized_test.fixedbin")
})

test_that("gintervals.summary with filtered test.sparse", {
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2), 0, -1))
    result <- gintervals.summary("test.sparse", intervs)
    expect_regression(result, "gintervals.summary_filtered_test.sparse")
})

test_that("gintervals.summary with randomized test.rects", {
    intervs <- gscreen("test.rects > 40", gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4)))
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]
    result <- gintervals.summary("test.rects", intervs)
    expect_regression(result, "gintervals.summary_randomized_test.rects")
})

test_that("gintervals.summary with filtered test.computed2d", {
    intervs <- gscreen("test.computed2d > 4000000", gintervals.2d(chroms1 = c(6, 1, 5), chroms2 = c(8, 1, 9)))
    result <- gintervals.summary("test.computed2d", intervs)
    expect_regression(result, "gintervals.summary_filtered_test.computed2d")
})

test_that("Summary of test.generated_1d_1 with various intervals and iterators", {
    result1 <- gintervals.summary("test.generated_1d_1", intervals = giterator.intervals("test.generated_1d_2"), iterator = giterator.intervals("test.generated_1d_1"))
    expect_regression(result1, "summary_test.generated_1d_1_case1")

    result2 <- gintervals.summary("test.generated_1d_1", intervals = giterator.intervals("test.generated_1d_2"), iterator = "test.bigintervs_1d_1")
    expect_regression(result2, "summary_test.generated_1d_1_case2")

    result3 <- gintervals.summary("test.generated_1d_1", intervals = giterator.intervals("test.generated_1d_2"), iterator = "test.generated_1d_1")
    expect_regression(result3, "summary_test.generated_1d_1_case3")

    result4 <- gintervals.summary("test.generated_1d_1", intervals = "test.bigintervs_1d_2", iterator = giterator.intervals("test.generated_1d_1"))
    expect_regression(result4, "summary_test.generated_1d_1_case4")

    result5 <- gintervals.summary("test.generated_1d_1", intervals = "test.bigintervs_1d_2", iterator = "test.bigintervs_1d_1")
    expect_regression(result5, "summary_test.generated_1d_1_case5")

    result6 <- gintervals.summary("test.generated_1d_1", intervals = "test.bigintervs_1d_2", iterator = "test.generated_1d_1")
    expect_regression(result6, "summary_test.generated_1d_1_case6")

    result7 <- gintervals.summary("test.generated_1d_1", intervals = "test.generated_1d_2", iterator = giterator.intervals("test.generated_1d_1"))
    expect_regression(result7, "summary_test.generated_1d_1_case7")

    result8 <- gintervals.summary("test.generated_1d_1", intervals = "test.generated_1d_2", iterator = "test.bigintervs_1d_1")
    expect_regression(result8, "summary_test.generated_1d_1_case8")

    result9 <- gintervals.summary("test.generated_1d_1", intervals = "test.generated_1d_2", iterator = "test.generated_1d_1")
    expect_regression(result9, "summary_test.generated_1d_1_case9")
})

test_that("Summary of test.generated_2d_5 with various intervals and iterators", {
    result1 <- gintervals.summary("test.generated_2d_5", intervals = giterator.intervals("test.generated_2d_6"), iterator = giterator.intervals("test.generated_2d_5"))
    expect_regression(result1, "summary_test.generated_2d_5_case1")

    result2 <- gintervals.summary("test.generated_2d_5", intervals = giterator.intervals("test.generated_2d_6"), iterator = "test.bigintervs_2d_5")
    expect_regression(result2, "summary_test.generated_2d_5_case2")

    result <- gintervals.summary("test.generated_2d_5", intervals = giterator.intervals("test.generated_2d_6"), iterator = "test.generated_2d_5")
    expect_regression(result, "summary_test.generated_2d_5_case3")

    result <- gintervals.summary("test.generated_2d_5", intervals = "test.bigintervs_2d_6", iterator = giterator.intervals("test.generated_2d_5"))
    expect_regression(result, "summary_test.generated_2d_5_case4")

    result <- gintervals.summary("test.generated_2d_5", intervals = "test.bigintervs_2d_6", iterator = "test.bigintervs_2d_5")
    expect_regression(result, "summary_test.generated_2d_5_case5")

    result <- gintervals.summary("test.generated_2d_5", intervals = "test.bigintervs_2d_6", iterator = "test.generated_2d_5")
    expect_regression(result, "summary_test.generated_2d_5_case6")

    result <- gintervals.summary("test.generated_2d_5", intervals = "test.generated_2d_6", iterator = giterator.intervals("test.generated_2d_5"))
    expect_regression(result, "summary_test.generated_2d_5_case7")

    result <- gintervals.summary("test.generated_2d_5", intervals = "test.generated_2d_6", iterator = "test.bigintervs_2d_5")
    expect_regression(result, "summary_test.generated_2d_5_case8")

    result9 <- gintervals.summary("test.generated_2d_5", intervals = "test.generated_2d_6", iterator = "test.generated_2d_5")
    expect_regression(result9, "summary_test.generated_2d_5_case9")
})

test_that("Testing gintervals for test.fixedbin", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    intervs1 <- gscreen("test.fixedbin > 0.2 & test.fixedbin < 0.3", gintervals(c(1, 2, 3), 0, -1))
    intervs2 <- gscreen("test.fixedbin > 0.25 & test.fixedbin < 0.35", gintervals(c(1, 2), 0, -1))
    set.seed(60427)
    intervs2 <- intervs2[sample(nrow(intervs2)), ]

    withr::with_options(list(gmax.data.size = nrow(intervs2) - 100), gintervals.summary("test.fixedbin", intervals = intervs2, iterator = intervs1, intervals.set.out = "test.testintervs"))
    result <- gintervals.load("test.testintervs")
    expect_regression(result, "gintervals_fixedbin_result")
})

test_that("Testing gintervals for test.rects", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    intervs <- gscreen("test.rects > 40", gintervals.2d(c(1, 2, 5, 8), 0, -1))
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]

    withr::with_options(list(gmax.data.size = nrow(intervs) - 100), gintervals.summary("test.rects", intervs, iterator = c(1, 1), intervals.set.out = "test.testintervs"))

    result <- gintervals.load("test.testintervs")
    expect_regression(result, "gintervals_rects_result")
})
