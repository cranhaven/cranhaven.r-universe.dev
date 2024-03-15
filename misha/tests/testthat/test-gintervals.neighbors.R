test_that("gintervals.neighbors works", {
    intervs <- gscreen("test.fixedbin > 0.3")
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]
    expect_regression(gintervals.neighbors("test.tss", intervs, 100, mindist = -10000, maxdist = 10000), "gintervals.neighbors.1")
    expect_regression(gintervals.neighbors("test.tss", intervs, 100, mindist = 2000, maxdist = 10000), "gintervals.neighbors.2")
    expect_regression(gintervals.neighbors("test.tss", intervs, 100, mindist = -10000, maxdist = -2000), "gintervals.neighbors.3")
    expect_regression(gintervals.neighbors(intervs, "test.tss", 100, mindist = -10000, maxdist = -2000), "gintervals.neighbors.4")
    expect_regression(gintervals.neighbors(gintervals.2d(1), gintervals.2d(1)), "gintervals.neighbors.5")
})

test_that("gintervals.neighbors works in 2D", {
    intervs1 <- gscreen("test.rects > 95")
    intervs2 <- gscreen("test.rects < 97 & test.rects > 94")
    intervs2$blabla <- 1:nrow(intervs2)
    expect_regression(gintervals.neighbors(intervs1, intervs2, 100, mindist1 = 10000, maxdist1 = 20000, mindist2 = 50000, maxdist2 = 70000), "gintervals.neighbors.2d.1")
    expect_regression(gintervals.neighbors("test.bigintervs_1d_1", "test.bigintervs_1d_2"), "gintervals.neighbors.2d.2")
    expect_regression(gintervals.neighbors("test.generated_1d_1", "test.generated_1d_2"), "gintervals.neighbors.2d.3")
    expect_regression(gintervals.neighbors("test.bigintervs_2d_5", "test.bigintervs_2d_6"), "gintervals.neighbors.2d.4")
    expect_regression(gintervals.neighbors("test.generated_2d_5", "test.generated_2d_6"), "gintervals.neighbors.2d.5")
})

test_that("gintervals.neighbors works with intervals.set.out", {
    gintervals.rm("test.testintervs_nei", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs_nei", force = TRUE))
    intervs <- gscreen("test.fixedbin > 0.3")
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]
    gintervals.neighbors("test.tss", intervs, 100, -10000, 10000, intervals.set.out = "test.testintervs_nei")
    expect_equal(
        gintervals.load("test.testintervs_nei") %>%
            tibble::repair_names() %>%
            arrange(chrom, start, end),
        gintervals.neighbors("test.tss", intervs, 100, -10000, 10000) %>%
            tibble::repair_names() %>%
            arrange(chrom, start, end)
    )
})

test_that("gintervals.neighbors works without min and max dist", {
    intervs <- gscreen("test.fixedbin > 0.3")
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]
    expect_regression(gintervals.neighbors(intervs, "test.tss", 1), "gintervals.neighbors.6")

    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2), 0, -1))
    expect_regression(gintervals.neighbors("test.tss", intervs, 1), "gintervals.neighbors.7")
})

test_that("columns are maintained", {
    intervs1 <- gscreen("test.fixedbin > 0.2 & test.fixedbin < 0.3", gintervals(c(1, 2), 0, -1))
    set.seed(60427)
    intervs1 <- intervs1[sample(nrow(intervs1)), ]
    intervs2 <- gscreen("test.fixedbin > 0.25 & test.fixedbin < 0.35", gintervals(c(1, 2), 0, -1))
    intervs2$usercol1 <- "aaa"
    intervs2$usercol2 <- 10 + (1:dim(intervs2)[1])
    res <- gintervals.neighbors(intervs1, intervs2, 1)
    expect_true(all(colnames(res) %in% c("chrom", "start", "end", "chrom1", "start1", "end1", "usercol1", "usercol2", "dist")))
    expect_true(all(c("usercol1", "usercol2") %in% colnames(res)))
    expect_regression(res, "gintervals.neighbors.8")
})

test_that("columns are maintained (2d)", {
    intervs1 <- gscreen("test.rects > 95")
    intervs2 <- gscreen("test.rects < 97 & test.rects > 94")
    intervs2$blabla <- 1:nrow(intervs2)
    res <- gintervals.neighbors(intervs1, intervs2, 1)
    expect_true(all(colnames(res) %in% c(
        "chrom1", "start1", "end1", "chrom2", "start2", "end2", "chrom11",
        "start11", "end11", "chrom21", "start21", "end21", "blabla",
        "dist1", "dist2"
    )))
    expect_true("blabla" %in% colnames(res))
    expect_regression(res, "gintervals.neighbors.9")
})

test_that("gintervals.neighbors works with intervals.set.out without min and max dist", {
    gintervals.rm("test.testintervs_nei", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs_nei", force = TRUE))
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2, 4), 0, -1))
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]
    gintervals.neighbors(intervs, "test.tss", 1, intervals.set.out = "test.testintervs_nei")
    expect_equal(
        gintervals.load("test.testintervs_nei") %>%
            tibble::repair_names() %>%
            arrange(chrom, start, end),
        gintervals.neighbors(intervs, "test.tss", 1) %>%
            tibble::repair_names() %>%
            arrange(chrom, start, end)
    )
})

test_that("gintervals.neighbors works with intervals.set.out with extra columns", {
    gintervals.rm("test.testintervs_nei", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs_nei", force = TRUE))
    intervs1 <- gscreen("test.fixedbin > 0.2 & test.fixedbin < 0.3", gintervals(c(1, 2), 0, -1))
    set.seed(60427)
    intervs1 <- intervs1[sample(nrow(intervs1)), ]
    intervs2 <- gscreen("test.fixedbin > 0.25 & test.fixedbin < 0.35", gintervals(c(1, 2), 0, -1))
    intervs2$usercol1 <- "aaa"
    intervs2$usercol2 <- 10 + (1:dim(intervs2)[1])
    gintervals.neighbors(intervs1, intervs2, 1, intervals.set.out = "test.testintervs_nei")
    expect_equal(
        gintervals.load("test.testintervs_nei") %>%
            tibble::repair_names() %>%
            arrange(chrom, start, end),
        gintervals.neighbors(intervs1, intervs2, 1) %>%
            tibble::repair_names() %>%
            arrange(chrom, start, end)
    )
})

test_that("gintervals.neighbors works with intervals.set.out (2d)", {
    gintervals.rm("test.testintervs_nei", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs_nei", force = TRUE))
    intervs1 <- gscreen("test.rects > 95")
    intervs2 <- gscreen("test.rects < 97 & test.rects > 94")
    set.seed(60427)
    intervs1 <- intervs1[sample(nrow(intervs1)), ]
    intervs2$blabla <- 1:nrow(intervs2)
    gintervals.neighbors(intervs1, intervs2, 1, intervals.set.out = "test.testintervs_nei")
    expect_equal(
        gintervals.load("test.testintervs_nei") %>%
            tibble::repair_names() %>%
            arrange(chrom1, start1, end1, chrom2, start2, end2),
        gintervals.neighbors(intervs1, intervs2, 1) %>%
            tibble::repair_names() %>%
            arrange(chrom1, start1, end1, chrom2, start2, end2)
    )
})
