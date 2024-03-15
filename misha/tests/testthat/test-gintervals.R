test_that("gintervals creation works", {
    expect_regression(gintervals(c(1, 2), c(0, 50, 2000, 50, 10000, 1500), c(100, 1300, 3000, 300, 11000, 2300)), "gintervals.creation.1")
    expect_error(gintervals(c(1, 2), c(0, 50, 2000, 50, 10000, 1500), c(100, 1300, 3000, 300, 11000, 2300), "a"))
    expect_error(gintervals(c(1, 2), c(0, 50, 2000, 50, 10000, 1500), c(100, 1300, 3000, 300, 11000, 2300), 25))
    expect_error(gintervals(c(1, 2), c(0, 50, 2000, 50, 10000, 1500), c(100, 1300, 3000, 300, 11000, 2300), 0.1))
    expect_regression(gintervals(c(1, 2), c(0, 50, 2000, 50, 10000, 1500), c(100, 1300, 3000, 300, 11000, 2300), -1), "gintervals.creation.2")
    expect_regression(gintervals.2d(c(1), c(0, 50), c(100, 200), c(3), c(0, 50), c(400, 600)), "gintervals.2d.creation.1")
    expect_regression(gintervals.2d(c(1, 2), c(0, 1000, 2000, 50, 10000, 1500), c(100, 1300, 3000, 300, 11000, 2300), c(3, 4), c(10, 1010, 2010, 60, 10010, 1510), c(110, 1310, 3010, 310, 11010, 2310)), "gintervals.2d.creation.2")
    expect_regression(gintervals.2d.all(), "gintervals.2d.all.1")
    expect_regression(gintervals.all(), "gintervals.all.1")
})

test_that("gintervals.2d.band_intersect works", {
    expect_regression(
        {
            intervs <- giterator.intervals("test.rects_big_rects", gintervals.2d(c(2, 3)), iterator = c(123450, 97891))
            set.seed(60427)
            intervs <- intervs[sample(nrow(intervs)), ]
            gintervals.2d.band_intersect(intervs, band = c(-198743, 23456))
        },
        "gintervals.2d.band_intersect.1"
    )
})


test_that("gintervals.2d.band_intersect (2)", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    intervs <- giterator.intervals("test.rects_big_rects", gintervals.2d(c(2, 3)), iterator = c(123450, 97891))
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]
    withr::with_options(list(gmax.data.size = 16000), gintervals.2d.band_intersect(intervs, band = c(-198743, 23456), intervals.set.out = "test.testintervs"))
    r <- gintervals.load("test.testintervs")
    expect_regression(r, "gintervals.2d.band_intersect.2")
})

test_that("gintervals.chrom_sizes work", {
    expect_regression(gintervals.chrom_sizes("bigintervs1d"), "gintervals.chrom_sizes.1")
    expect_regression(gintervals.chrom_sizes("bigintervs2d"), "gintervals.chrom_sizes.2")
    expect_regression(gintervals.chrom_sizes("test.tss"), "gintervals.chrom_sizes.3")
    expect_regression(gintervals.chrom_sizes("test.array"), "gintervals.chrom_sizes.4")
    expect_error(gintervals.chrom_sizes("test.fixedbin"))
    expect_regression(gintervals.chrom_sizes("test.sparse"), "gintervals.chrom_sizes.5")
    expect_regression(gintervals.chrom_sizes("test.rects"), "gintervals.chrom_sizes.6")
})

test_that("gintervals.diff works", {
    intervs1 <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2), 0, -1))
    intervs2 <- gscreen("test.fixedbin > 0.4", gintervals(c(1, 2), 0, -1))
    expect_regression(gintervals.diff(intervs1, intervs2), "gintervals.diff.1")
})

test_that("gintervals.diff works (2)", {
    intervs1 <- gscreen("test.fixedbin > 0.1 & test.fixedbin < 0.3", gintervals(c(1, 2)))
    intervs2 <- gscreen("test.fixedbin > 0.2 & test.fixedbin < 0.4", gintervals(c(1, 2)))
    intervs3 <- gscreen("(test.fixedbin > 0.25 & test.fixedbin < 0.32) | test.fixedbin > 0.35", gintervals(c(1, 2)))
    expect_regression(gintervals.diff(rbind(intervs1, intervs2), intervs3), "gintervals.diff.2")
})

test_that("gintervals.diff works (3)", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    intervs1 <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2, 4, 8, 9), 0, -1))
    intervs2 <- gscreen("test.fixedbin > 0.4", gintervals(c(1, 2, 4, 7, 9), 0, -1))
    max.data.size <- getOption("gmax.data.size")
    withr::with_options(list(gmax.data.size = 1300000), gintervals.diff(intervs1, intervs2, intervals.set.out = "test.testintervs"))
    expect_regression(gintervals.load("test.testintervs"), "gintervals.diff.3")
})

test_that("gintervals.load works", {
    expect_regression(gintervals.load("test.foodgene"), "gintervals.load.1")
    expect_regression(gintervals.load("bigintervs1d"), "gintervals.load.2")
    expect_regression(gintervals.load("bigintervs1d", chrom = 2), "gintervals.load.3")
    expect_regression(gintervals.load("bigintervs2d"), "gintervals.load.4")
    expect_regression(gintervals.load("bigintervs2d", chrom1 = 2, chrom2 = 2), "gintervals.load.5")
    expect_regression(gintervals.load("test.rects", chrom1 = 1, chrom2 = 2), "gintervals.load.6")
    expect_regression(gintervals.load("test.generated_1d_1", chrom = 13), "gintervals.load.7")
    expect_regression(gintervals.load("test.generated_1d_1", chrom = 12), "gintervals.load.8")
    expect_regression(gintervals.load("test.generated_2d_5", chrom1 = 1, chrom2 = 2), "gintervals.load.9")
    expect_regression(gintervals.load("test.generated_2d_5", chrom1 = 1, chrom2 = 3), "gintervals.load.10")
})

test_that("gmax.data.size works", {
    expect_error(withr::with_options(list(gmax.data.size = 200000), gintervals.load("test.generated_1d_2")))
    expect_error(withr::with_options(list(gmax.data.size = 100000), gintervals.load("test.generated_1d_2")))
    expect_error(withr::with_options(list(gmax.data.size = 1000000), gintervals.load("test.test.generated_2d_6")))
    expect_error(withr::with_options(list(gmax.data.size = 100), gintervals.load("test.test.generated_2d_6")))
})

test_that("gintervals.ls works", {
    expect_equal(gintervals.ls(), c(
        "allgenome_big", "allgenome_big_2d", "bigintervs1d", "bigintervs2d",
        "bigset1d", "global.exon", "global.foodgene", "global.i2d_1",
        "global.test", "global.tss", "global.z1", "intervs.foodgene",
        "intervs.i", "test.bigintervs_1d_1", "test.bigintervs_1d_2",
        "test.bigintervs_2d_1", "test.bigintervs_2d_2", "test.bigintervs_2d_5",
        "test.bigintervs_2d_6", "test.foodgene", "test.testintervs2",
        "test.testintervs20", "test.tss", "testintervs", "testintervs1",
        "testintervs17", "testintervs2", "testintervs3"
    ))
    expect_equal(gintervals.ls("test"), c(
        "global.test", "test.bigintervs_1d_1", "test.bigintervs_1d_2",
        "test.bigintervs_2d_1", "test.bigintervs_2d_2", "test.bigintervs_2d_5",
        "test.bigintervs_2d_6", "test.foodgene", "test.testintervs2",
        "test.testintervs20", "test.tss", "testintervs", "testintervs1",
        "testintervs17", "testintervs2", "testintervs3"
    ))
})

test_that("gintervals.load works (2)", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    withr::with_options(list(data.size = 6000000), gextract("test.fixedbin", gintervals(c(1, 2)), intervals.set.out = "test.testintervs"))
    expect_regression(gintervals.load("test.testintervs"), "gintervals.1")
})

test_that("gextract with gmax.data.size set to 300000 works", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    withr::with_options(list(gmax.data.size = 300000), gextract("test.rects", gintervals.2d(c(1, 2)), intervals.set.out = "test.testintervs"))
    expect_regression(gintervals.load("test.testintervs"), "gintervals.2")
})

test_that("glookup with gmax.data.size set to 6000000 works", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    m1 <- matrix(1:15, nrow = 5, ncol = 3)
    withr::with_options(list(gmax.data.size = 6000000), glookup(m1, "test.fixedbin", seq(0.1, 0.2, length.out = 6), "test.sparse", seq(0.25, 0.48, length.out = 4), gintervals(c(1, 2)), iterator = "test.fixedbin", intervals.set.out = "test.testintervs"))
    expect_regression(gintervals.load("test.testintervs"), "gintervals.3")
})

test_that("glookup with gmax.data.size set to 100000 works", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    m1 <- matrix(1:15, nrow = 5, ncol = 3)
    withr::with_options(list(gmax.data.size = 100000), glookup(m1, "test.computed2d", seq(5000000, 10000000, length.out = 6), "test.computed2d / 2", seq(0, 4000000, length.out = 4), gintervals.2d(chroms1 = c(6, 5), chroms2 = c(8, 9)), force.binning = FALSE, intervals.set.out = "test.testintervs"))
    expect_regression(gintervals.load("test.testintervs"), "gintervals.4")
})

test_that("gscreen with gmax.data.size set to 1000000 works", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    withr::with_options(list(gmax.data.size = 1000000), gscreen("2 * test.sparse+0.2 > 0.4", intervals.set.out = "test.testintervs"))
    expect_regression(gintervals.load("test.testintervs"), "gintervals.5")
})

test_that("gscreen with gmax.data.size set to 130000 works", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    withr::with_options(list(gmax.data.size = 130000), gscreen("test.rects > 40", intervals.set.out = "test.testintervs"))
    expect_regression(gintervals.load("test.testintervs"), "gintervals.6")
})

test_that("gintervals.diff works", {
    expect_regression(gintervals.diff("test.bigintervs_1d_1", "test.bigintervs_1d_2"), "gintervals.diff.binintervs")
})


test_that("gintervals.exists checks correctly", {
    expect_true(gintervals.exists("test.tss"))
    expect_false(gintervals.exists("test.blablablablabla"))
    expect_false(gintervals.exists("blablablablabla.blablablablabla"))
})

test_that("gintervals.force_range handles 1D data correctly", {
    expect_equal(gintervals.force_range(rbind(
        data.frame(chrom = "chr1", start = 10, end = 100),
        data.frame(chrom = "chr1", start = 300, end = 200),
        data.frame(chrom = "chr1", start = -100, end = 50),
        data.frame(chrom = "chr1", start = -100, end = -30),
        data.frame(chrom = "chr1", start = -30, end = -100),
        data.frame(chrom = "chr1", start = 100, end = 1e+09),
        data.frame(chrom = "chr1", start = 1e+09, end = 10 + 1e+09),
        data.frame(chrom = "chr1", start = 10 + 1e+09, end = 1e+09)
    )), structure(list(chrom = c("chr1", "chr1", "chr1"), start = c(
        10,
        0, 100
    ), end = c(100, 50, 247249719)), row.names = c(NA, 3L), class = "data.frame"))
})

test_that("gintervals.force_range handles 2D data correctly", {
    expect_equal(gintervals.force_range(rbind(
        data.frame(chrom1 = "chr1", start1 = 10, end1 = 100, chrom2 = "chr2", start2 = 10, end2 = 100),
        data.frame(chrom1 = "chr1", start1 = 300, end1 = 200, chrom2 = "chr2", start2 = 300, end2 = 200),
        data.frame(chrom1 = "chr1", start1 = -100, end1 = 50, chrom2 = "chr2", start2 = -100, end2 = 50),
        data.frame(chrom1 = "chr1", start1 = -100, end1 = -30, chrom2 = "chr2", start2 = -100, end2 = -30),
        data.frame(chrom1 = "chr1", start1 = -30, end1 = -100, chrom2 = "chr2", start2 = -30, end2 = -100),
        data.frame(chrom1 = "chr1", start1 = 100, end1 = 1e+09, chrom2 = "chr2", start2 = 100, end2 = 1e+09),
        data.frame(chrom1 = "chr1", start1 = 1e+09, end1 = 10 + 1e+09, chrom2 = "chr2", start2 = 1e+09, end2 = 10 + 1e+09),
        data.frame(chrom1 = "chr1", start1 = 10 + 1e+09, end1 = 1e+09, chrom2 = "chr2", start2 = 10 + 1e+09, end2 = 1e+09)
    )), structure(list(chrom1 = c("chr1", "chr1", "chr1"), start1 = c(
        10,
        0, 100
    ), end1 = c(100, 50, 247249719), chrom2 = c(
        "chr2", "chr2",
        "chr2"
    ), start2 = c(10, 0, 100), end2 = c(100, 50, 242951149)), row.names = c(
        NA,
        3L
    ), class = "data.frame"))
})

test_that("gintervals.is.bigset checks bigintervs1d", {
    withr::with_options(list(gmax.data.size = 100), {
        expect_true(gintervals.is.bigset("bigintervs1d"))
    })
})

test_that("gintervals.is.bigset checks bigintervs2d", {
    withr::with_options(list(gmax.data.size = 100), {
        expect_true(gintervals.is.bigset("bigintervs2d"))
    })
})

test_that("gintervals.is.bigset checks test.tss", {
    withr::with_options(list(gmax.data.size = 100), {
        expect_false(gintervals.is.bigset("test.tss"))
    })
})

test_that("gintervals.rbind with saved data works", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    intervs1 <- gextract("test.fixedbin", gintervals(c(1, 2), 1000, 4000))
    intervs2 <- gextract("test.fixedbin", gintervals(c(2, "X"), 2000, 5000))
    gintervals.save("test.testintervs", intervs2)
    r <- gintervals.rbind(intervs1, "test.testintervs")
    expect_true(!is.null(r))
    expect_regression(r, "gintervals.rbind.1")
})

test_that("gintervals.ls updates after save and remove operations", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    gintervals.save("test.testintervs", gintervals(c(1, 2)))
    r1 <- gintervals.ls()
    r2 <- gintervals.ls()
    expect_regression(list(r1, r2), "gintervals.rbind.2")
})

test_that("gextract with removed interval gives an error", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    gintervals.save("test.testintervs", gintervals(c(1, 2), 1000, 2000))
    gintervals.rm("test.testintervs", force = TRUE)
    expect_error(gextract("test.fixedbin", "test.testintervs"))
})

test_that("gintervals.rm handles non-existent data without error when using force", {
    expect_silent(gintervals.rm("test.aaaaaaaaaaaaaaaaaaa", force = TRUE))
    expect_error(gintervals.rm("test.aaaaaaaaaaaaaaaaaaa"))
})

test_that("gintervals.ls reflects changes after save", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    r1 <- gintervals.ls()
    gintervals.save("test.testintervs", gintervals(c(1, 2), 1000, 2000))
    r2 <- gintervals.ls()
    expect_regression(list(r1, r2), "gintervals.ls.1")
})

test_that("gextract works with saved intervals", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))
    gintervals.save("test.testintervs", gintervals(c(1, 2), 1000, 2000))
    r <- gextract("test.fixedbin", "test.testintervs")
    expect_true(!is.null(r))
    expect_regression(r, "gintervals.save.1")
})

test_that("gscreen and gintervals.union works correctly", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))

    intervs1 <- gscreen("test.fixedbin > 0.1 & test.fixedbin < 0.3", gintervals(c(1, 2, 4, 8, 9), 0, -1))
    intervs2 <- gscreen("test.fixedbin < 0.2", gintervals(c(1, 2, 4, 7, 9), 0, -1))

    withr::with_options(list(gmax.data.size = 1000000), {
        gintervals.union(intervs1, intervs2, intervals.set.out = "test.testintervs")
    })

    r <- gintervals.load("test.testintervs")
    expect_true(!is.null(r))
    expect_regression(r, "gscreen_and_gintervals.union.1")
})

test_that("gintervals.update with saved data using chrom 1", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))

    chrom_size_limit <- sum(gintervals.chrom_sizes("bigintervs1d")$size) + 100

    withr::with_options(list(gmax.data.size = chrom_size_limit), {
        gintervals.save("test.testintervs", "bigintervs1d")
    })

    expect_error(gintervals.update("test.testintervs", gintervals(1), chrom = 1))
})

test_that("gintervals.update with loaded data using chrom 1", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))

    chrom_size_limit <- sum(gintervals.chrom_sizes("bigintervs1d")$size) + 100

    withr::with_options(list(gmax.data.size = chrom_size_limit), {
        gintervals.save("test.testintervs", "bigintervs1d")
    })

    r <- gintervals.load("test.testintervs", chrom = 2)
    expect_error(gintervals.update("test.testintervs", r[c(2, 3), ], chrom1 = 1))
})

test_that("gintervals.update with loaded data using chrom 2", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))

    chrom_size_limit <- sum(gintervals.chrom_sizes("bigintervs1d")$size) + 100

    withr::with_options(list(gmax.data.size = chrom_size_limit), {
        gintervals.save("test.testintervs", "bigintervs1d")
    })

    r <- gintervals.load("test.testintervs", chrom = 2)
    gintervals.update("test.testintervs", r[c(2, 3), ], chrom = 2)
    result <- list(gintervals.load("test.testintervs", chrom = 2), gintervals.chrom_sizes("test.testintervs"))
    expect_regression(result, "gintervals.update.3")
})

test_that("gintervals.update removes chrom 2 from saved data", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))

    chrom_size_limit <- sum(gintervals.chrom_sizes("bigintervs1d")$size) + 100

    withr::with_options(list(gmax.data.size = chrom_size_limit), {
        gintervals.save("test.testintervs", "bigintervs1d")
    })

    gintervals.update("test.testintervs", NULL, chrom = 2)
    result <- list(gintervals.load("test.testintervs", chrom = 2), gintervals.chrom_sizes("test.testintervs"))
    expect_regression(result, "gintervals.update.4")
})

test_that("gintervals.update with saved 2d data using chrom1 1", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))

    chrom_size_limit <- sum(gintervals.chrom_sizes("bigintervs2d")$size) + 100

    withr::with_options(list(gmax.data.size = chrom_size_limit), {
        gintervals.save("test.testintervs", "bigintervs2d")
    })

    expect_error(gintervals.update("test.testintervs", gintervals.2d(1), chrom1 = 1))
})

test_that("gintervals.update with loaded 2d data using chrom 1", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))

    chrom_size_limit <- sum(gintervals.chrom_sizes("bigintervs2d")$size) + 100

    withr::with_options(list(gmax.data.size = chrom_size_limit), {
        gintervals.save("test.testintervs", "bigintervs2d")
    })

    r <- gintervals.load("test.testintervs", chrom1 = 2, chrom2 = 2)
    expect_error(gintervals.update("test.testintervs", r[c(2, 3), ], chrom = 1))
})

test_that("gintervals.update with loaded 2d data using chrom1 2 and chrom2 2", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))

    chrom_size_limit <- sum(gintervals.chrom_sizes("bigintervs2d")$size) + 100

    withr::with_options(list(gmax.data.size = chrom_size_limit), {
        gintervals.save("test.testintervs", "bigintervs2d")
    })

    r <- gintervals.load("test.testintervs", chrom1 = 2, chrom2 = 2)
    gintervals.update("test.testintervs", r[c(2, 3), ], chrom1 = 2, chrom2 = 2)
    result <- list(gintervals.load("test.testintervs", chrom1 = 2, chrom2 = 2), gintervals.chrom_sizes("test.testintervs"))
    expect_regression(result, "gintervals.update.2d.3")
})

test_that("gintervals.update removes chrom1 2 and chrom2 2 from saved 2d data", {
    gintervals.rm("test.testintervs", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs", force = TRUE))

    chrom_size_limit <- sum(gintervals.chrom_sizes("bigintervs2d")$size) + 100

    withr::with_options(list(gmax.data.size = chrom_size_limit), {
        gintervals.save("test.testintervs", "bigintervs2d")
    })

    gintervals.update("test.testintervs", NULL, chrom1 = 2, chrom2 = 2)
    result <- list(gintervals.load("test.testintervs", chrom1 = 2, chrom2 = 2), gintervals.chrom_sizes("test.testintervs"))
    expect_regression(result, "gintervals.update.2d.4")
})
