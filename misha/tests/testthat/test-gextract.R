test_that("gextract with fixedbin track works", {
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2)))
    expect_regression(gextract("test.fixedbin", intervs), "gextract.fixedbin")
})

test_that("gextract with sparse works", {
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2)))
    expect_regression(gextract("test.sparse", intervs), "gextract.sparse")
})

test_that("gextract with array works", {
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2)))
    expect_regression(gextract("test.array", intervs), "gextract.array")
})

test_that("gextract with computed2d works", {
    intervs <- gscreen("test.rects > 9")
    expect_regression(gextract("test.computed2d", intervs), "gextract.computed2d.1")
    intervs <- gscreen("test.computed2d > 9000000")
    expect_regression(gextract("test.computed2d", intervs), "gextract.computed2d.2")
})

test_that("gextract with .misha$ALLGENOME works", {
    withr::local_options(gmax.data.size = 1e9)
    expect_regression(gextract("test.fixedbin", .misha$ALLGENOME), "gextract.allgenome.fixedbin")
    expect_regression(gextract("test.sparse", .misha$ALLGENOME), "gextract.allgenome.sparse")
    expect_regression(gextract("test.array", .misha$ALLGENOME), "gextract.allgenome.array")
    expect_regression(gextract("test.rects", .misha$ALLGENOME), "gextract.allgenome.rects")
    expect_regression(gextract("test.computed2d", .misha$ALLGENOME), "gextract.allgenome.computed2d")
})

test_that("gextract with sparse track can save to a file", {
    tmp <- tempfile()
    gextract("test.sparse", gintervals(c(1, 2)), file = tmp)
    r <- readr::read_tsv(tmp, col_types = readr::cols(
        chrom = readr::col_character(),
        start = readr::col_double(),
        end = readr::col_double(),
        test.sparse = readr::col_double()
    ))
    r1 <- gextract("test.sparse", gintervals(c(1, 2))) %>%
        mutate(chrom = as.character(chrom)) %>%
        select(-intervalID)
    expect_equal(r, r1, ignore_attr = TRUE)
})


test_that("gextract with specific intervals works", {
    withr::local_options(gmax.data.size = 1e9)
    expect_regression(gextract("test.fixedbin", .misha$ALLGENOME), "gextract.allgenome.fixedbin")
    expect_regression(gextract("test.sparse", .misha$ALLGENOME), "gextract.allgenome.sparse")
    expect_regression(gextract("test.array", .misha$ALLGENOME), "gextract.allgenome.array")
    expect_regression(gextract("test.rects", .misha$ALLGENOME), "gextract.allgenome.rects")
    expect_regression(gextract("test.computed2d", .misha$ALLGENOME), "gextract.allgenome.computed2d")
})

test_that("gextract iterators", {
    expect_regression(gextract("test.fixedbin", gintervals(c(1, 2), 0, 1000000)), "gextract.65")
    expect_regression(gextract("test.sparse", gintervals(c(1, 2), 0, 1000000)), "gextract.64")
    expect_regression(gextract("test.array", gintervals(c(1, 2), 0, 1000000)), "gextract.63")
    expect_regression(gextract("test.rects", gintervals.2d(c(2, 3), 10000000, 50000000, c(2, 4), 30000000, 80000000)), "gextract.62")
    expect_error(gextract("test.rects", intervals = gintervals.2d(c(1), c(0, 50), c(100, 200), c(3), c(0, 50), c(400, 600))))
    expect_error(gextract("test.rects", .misha$ALLGENOME, iterator = gintervals.2d(c(1), c(0, 50), c(100, 200), c(3), c(0, 50), c(400, 600))))
    expect_regression(gextract("test.rects_big_rects", gintervals.2d(c(1:20)), band = c(-1874356, 234560)), "gextract.59")
    expect_regression(gextract("test.computed2d", gintervals.2d(c(6, 8), 10000000, 50000000, c(1, 3), 30000000, 80000000)), "gextract.58")
    expect_error(gextract("test.computed2d", intervals = gintervals.2d(c(1), c(0, 50), c(100, 200), c(3), c(0, 50), c(400, 600))))
    expect_error(gextract("test.computed2d", .misha$ALLGENOME, iterator = gintervals.2d(c(1), c(0, 50), c(100, 200), c(3), c(0, 50), c(400, 600))))
    expect_regression(gextract("test.computed2d", gintervals.2d(c(1:20)), band = c(-1874356, 234560)), "gextract.55")
    expect_regression(gextract("2 * test.fixedbin + 17", gintervals(c(1, 2), 0, 1000000)), "gextract.54")
    expect_regression(gextract("2 * test.sparse + 17", gintervals(c(1, 2), 0, 1000000)), "gextract.53")
    expect_regression(gextract("2 * test.array + 17", gintervals(c(1, 2), 0, 1000000)), "gextract.52")
    expect_regression(gextract("2 * test.rects + 17", gintervals.2d(c(2, 3), 10000000, 50000000, c(2, 4), 30000000, 80000000)), "gextract.51")
    expect_regression(gextract("2 * test.computed2d + 17", gintervals.2d(c(6, 8), 10000000, 50000000, c(1, 3), 30000000, 80000000)), "gextract.50")
    expect_error(gextract("2 * test.fixedbin + 17 > 10", gintervals(c(1, 2), 0, 1000000)))
    expect_error(gextract("test.fixedbin", "test.sparse", gintervals(c(1, 2), 0, 1000000)))
    expect_regression(gextract("test.fixedbin", "test.sparse", gintervals(c(1, 2), 0, 1000000), iterator = "test.fixedbin"), "gextract.47")
    expect_error(gextract("test.fixedbin", "test.array", gintervals(c(1, 2), 0, 1000000)))
    expect_regression(gextract("test.fixedbin", "test.array", gintervals(c(1, 2), 0, 1000000), iterator = "test.fixedbin"), "gextract.45")
    expect_regression(gextract("test.rects", "test.rects * 3", gintervals.2d(c(2, 3), 10000000, 50000000, c(2, 4), 30000000, 80000000), iterator = "test.rects"), "gextract.44")
    expect_regression(gextract("test.computed2d", "test.computed2d * 3", gintervals.2d(c(6, 5), 10000000, 50000000, c(8, 9), 30000000, 80000000), iterator = "test.computed2d"), "gextract.43")
    expect_regression(gextract("test.rects", "test.rects * 3", gintervals.2d(c(6, 5), 10000000, 50000000, c(8, 9), 30000000, 80000000), iterator = "test.computed2d"), "gextract.42")
    expect_regression(gextract("test.computed2d", "test.computed2d * 3", gintervals.2d(c(6, 5), 10000000, 50000000, c(8, 9), 30000000, 80000000), iterator = "test.rects"), "gextract.41")
    expect_regression(gextract("test.fixedbin", gintervals(c(2, 3))), "gextract.40")
    expect_regression(gextract("test.sparse", gintervals(c(2, 3))), "gextract.39")
    expect_regression(gextract("test.array", gintervals(c(2, 3))), "gextract.38")
    expect_regression(gextract("test.rects", gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4))), "gextract.37")
    expect_regression(gextract("test.computed2d", gintervals.2d(chroms1 = c(6, 5), chroms2 = c(8, 9))), "gextract.36")
    expect_regression(gextract("test.fixedbin", gintervals(c(2, 3)), iterator = 120), "gextract.35")
    expect_regression(gextract("test.sparse", gintervals(c(2, 3)), iterator = 120), "gextract.34")
    expect_regression(gextract("test.array", gintervals(c(2, 3)), iterator = 120), "gextract.33")
    expect_error(gextract("test.rects", gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4)), iterator = 120))
    expect_error(gextract("test.computed2d", gintervals.2d(chroms1 = c(6, 5), chroms2 = c(8, 9)), iterator = 120))
    expect_error(gextract("test.fixedbin", gintervals(c(2, 3)), iterator = c(100000, 100000)))
    expect_error(gextract("test.sparse", gintervals(c(2, 3)), iterator = c(100000, 100000)))
    expect_error(gextract("test.array", gintervals(c(2, 3)), iterator = c(100000, 100000)))
    expect_regression(gextract("test.rects", gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4)), iterator = c(100000, 100000)), "gextract.27")
    expect_regression(gextract("test.computed2d", gintervals.2d(chroms1 = c(6, 5), chroms2 = c(8, 9)), iterator = c(100000, 100000)), "gextract.26")
    expect_regression(gextract("test.fixedbin", gintervals(c(2, 3)), iterator = "test.fixedbin"), "gextract.25")
    expect_regression(gextract("test.sparse", gintervals(c(2, 3)), iterator = "test.fixedbin"), "gextract.24")
    expect_regression(gextract("test.array", gintervals(c(2, 3)), iterator = "test.fixedbin"), "gextract.23")
    expect_error(gextract("test.rects", gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4)), iterator = "test.fixedbin"))
    expect_error(gextract("test.computed2d", gintervals.2d(chroms1 = c(6, 5), chroms2 = c(8, 9)), iterator = "test.fixedbin"))
    expect_regression(gextract("test.fixedbin", gintervals(c(2, 3)), iterator = "test.sparse"), "gextract.20")
    expect_regression(gextract("test.sparse", gintervals(c(2, 3)), iterator = "test.sparse"), "gextract.19")
    expect_regression(gextract("test.array", gintervals(c(2, 3)), iterator = "test.sparse"), "gextract.18")
    expect_error(gextract("test.rects", gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4)), iterator = "test.sparse"))
    expect_error(gextract("test.computed2d", gintervals.2d(chroms1 = c(6, 5), chroms2 = c(8, 9)), iterator = "test.sparse"))
    expect_regression(gextract("test.fixedbin", gintervals(c(2, 3)), iterator = "test.array"), "gextract.15")
    expect_regression(gextract("test.sparse", gintervals(c(2, 3)), iterator = "test.array"), "gextract.14")
    expect_regression(gextract("test.array", gintervals(c(2, 3)), iterator = "test.array"), "gextract.13")
    expect_error(gextract("test.rects", gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4)), iterator = "test.array"))
    expect_error(gextract("test.computed2d", gintervals.2d(chroms1 = c(6, 5), chroms2 = c(8, 9)), iterator = "test.array"))
    expect_error(gextract("test.fixedbin", gintervals(c(2, 3)), iterator = "test.rects"))
    expect_error(gextract("test.sparse", gintervals(c(2, 3)), iterator = "test.rects"))
    expect_error(gextract("test.array", gintervals(c(2, 3)), iterator = "test.rects"))
    expect_regression(gextract("test.rects", gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4)), iterator = "test.rects"), "gextract.7")
    expect_regression(gextract("test.computed2d", gintervals.2d(chroms1 = c(6, 5), chroms2 = c(8, 9)), iterator = "test.rects"), "gextract.6")
    expect_error(gextract("test.fixedbin", gintervals(c(2, 3)), iterator = "test.computed2d"))
    expect_error(gextract("test.sparse", gintervals(c(2, 3)), iterator = "test.computed2d"))
    expect_error(gextract("test.array", gintervals(c(2, 3)), iterator = "test.computed2d"))
    expect_regression(gextract("test.rects", gintervals.2d(chroms1 = c(6, 5), chroms2 = c(8, 9)), iterator = "test.computed2d"), "gextract.2")
    expect_regression(gextract("test.computed2d", gintervals.2d(chroms1 = c(6, 5), chroms2 = c(8, 9)), iterator = "test.computed2d"), "gextract.1")
})

test_that("gextract works with gscreen 1d", {
    intervs1 <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2)))
    intervs2 <- gscreen("test.fixedbin < 0.4", gintervals(c(1, 2)))
    expect_regression(gextract("test.fixedbin", intervs1, iterator = intervs2), "gextract.gscreen.1d.fixedbin")
    expect_regression(gextract("test.sparse", intervs1, iterator = intervs2), "gextract.gscreen.1d.sparse")
    expect_regression(gextract("test.array", intervs1, iterator = intervs2), "gextract.gscreen.1d.array")
    expect_error(gextract("test.rects", intervs1, iterator = intervs2))
    expect_error(gextract("test.computed2d", intervs1, iterator = intervs2))
})

test_that("gextract works with gscreen 2d", {
    intervs1 <- gscreen("test.rects > 40", gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4)))
    intervs2 <- gscreen("test.rects < 50", gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4)))
    expect_error(gextract("test.fixedbin", intervs1, iterator = intervs2))
    expect_error(gextract("test.sparse", intervs1, iterator = intervs2))
    expect_error(gextract("test.array", intervs1, iterator = intervs2))
    expect_regression(gextract("test.rects", intervs1, iterator = intervs2), "gextract.gscreen.2d.rects")
    expect_regression(gextract("test.computed2d", intervs1, iterator = intervs2), "gextract.gscreen.2d.computed2d")
})

test_that("gextract works with 2d .misha$ALLGENOME", {
    intervs <- rbind(
        gintervals.2d(1, 10, 100, 1, 10, 100),
        gintervals.2d(1, 400, 500, 1, 400, 500),
        gintervals.2d(2, 600, 700, 2, 600, 700),
        gintervals.2d(1, 200, 300, 2, 200, 300),
        gintervals.2d(1, 7000, 9100, "X", 7000, 9100),
        gintervals.2d(2, 9000, 18000, 2, 9000, 18000),
        gintervals.2d(1, 30000, 31000, 1, 30000, 31000),
        gintervals.2d(2, 1130, 15000, 1, 1130, 15000),
        gintervals.2d(1, 1100, 1120, 1, 1100, 1120),
        gintervals.2d(1, 1000, 1100, 2, 1000, 1100)
    )
    expect_regression(gextract("test.rects", intervals = .misha$ALLGENOME, iterator = intervs), "gextract.2d.ALLGENOME.rects")
})

test_that("gextract with giterator.intervals works", {
    expect_regression(gextract("test.generated_1d_1", intervals = giterator.intervals("test.generated_1d_2"), iterator = giterator.intervals("test.generated_1d_1")), "gextract.giterator.intervals.1")
    expect_regression(gextract("test.generated_1d_1", intervals = giterator.intervals("test.generated_1d_2"), iterator = "test.bigintervs_1d_1"), "gextract.giterator.intervals.2")
    expect_regression(gextract("test.generated_1d_1", intervals = giterator.intervals("test.generated_1d_2"), iterator = "test.generated_1d_1"), "gextract.giterator.intervals.3")
    expect_regression(gextract("test.generated_1d_1", intervals = "test.bigintervs_1d_2", iterator = giterator.intervals("test.generated_1d_1")), "gextract.giterator.intervals.4")
    expect_regression(gextract("test.generated_1d_1", intervals = "test.bigintervs_1d_2", iterator = "test.bigintervs_1d_1"), "gextract.giterator.intervals.5")
    expect_regression(gextract("test.generated_1d_1", intervals = "test.bigintervs_1d_2", iterator = "test.generated_1d_1"), "gextract.giterator.intervals.6")
    expect_regression(gextract("test.generated_1d_1", intervals = "test.generated_1d_2", iterator = giterator.intervals("test.generated_1d_1")), "gextract.giterator.intervals.7")
    expect_regression(gextract("test.generated_1d_1", intervals = "test.generated_1d_2", iterator = "test.bigintervs_1d_1"), "gextract.giterator.intervals.8")
    expect_regression(gextract("test.generated_1d_1", intervals = "test.generated_1d_2", iterator = "test.generated_1d_1"), "gextract.giterator.intervals.9")
    expect_regression(gextract("test.generated_2d_5", intervals = giterator.intervals("test.generated_2d_6"), iterator = giterator.intervals("test.generated_2d_5")), "gextract.giterator.intervals.10")
    expect_regression(gextract("test.generated_2d_5", intervals = giterator.intervals("test.generated_2d_6"), iterator = "test.bigintervs_2d_5"), "gextract.giterator.intervals.11")
    expect_regression(gextract("test.generated_2d_5", intervals = giterator.intervals("test.generated_2d_6"), iterator = "test.generated_2d_5"), "gextract.giterator.intervals.12")
    expect_regression(gextract("test.generated_2d_5", intervals = "test.bigintervs_2d_6", iterator = giterator.intervals("test.generated_2d_5")), "gextract.giterator.intervals.13")
    expect_regression(gextract("test.generated_2d_5", intervals = "test.bigintervs_2d_6", iterator = "test.bigintervs_2d_5"), "gextract.giterator.intervals.14")
    expect_regression(gextract("test.generated_2d_5", intervals = "test.bigintervs_2d_6", iterator = "test.generated_2d_5"), "gextract.giterator.intervals.15")
    expect_regression(gextract("test.generated_2d_5", intervals = "test.generated_2d_6", iterator = giterator.intervals("test.generated_2d_5")), "gextract.giterator.intervals.16")
    expect_regression(gextract("test.generated_2d_5", intervals = "test.generated_2d_6", iterator = "test.bigintervs_2d_5"), "gextract.giterator.intervals.17")
    expect_regression(gextract("test.generated_2d_5", intervals = "test.generated_2d_6", iterator = "test.generated_2d_5"), "gextract.giterator.intervals.18")
})
