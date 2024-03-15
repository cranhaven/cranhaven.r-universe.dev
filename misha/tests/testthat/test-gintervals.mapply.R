test_that("gintervals.mapply works", {
    expect_equal(
        gintervals.mapply(function(x) {
            max(x + 2, na.rm = TRUE)
        }, "test.fixedbin", gintervals(c(1, 2), 0, 100000)) %>% mutate(chrom = as.character(chrom)),
        data.frame(
            chrom = c("chr1", "chr2"),
            start = c(0, 0),
            end = c(100000, 100000),
            value = c(2.54000002145767, 2.46000000834465)
        ),
        ignore_attr = TRUE
    )

    expect_regression(gintervals.mapply(function(x) {
        max(x + 2, na.rm = TRUE)
    }, "test.fixedbin", .misha$ALLGENOME), "gintervals.mapply.fixedbin.ALLGENOME")

    expect_regression(gintervals.mapply(function(x) {
        max(x + 2, na.rm = TRUE)
    }, "test.generated_1d_1", "test.bigintervs_1d_1"), "gintervals.mapply.test.generated_1d_1.test.bigintervs_1d_1")
})

test_that("gintervals.mapply fails with 1d function for 2d intervals", {
    expect_error(gintervals.mapply(function(x) {
        max(x + 2, na.rm = TRUE)
    }, "test.rects", .misha$ALLGENOME, iterator = "test.rects"))
    expect_error(gintervals.mapply(function(x) {
        max(x + 2, na.rm = TRUE)
    }, "test.generated_2d_5", "test.bigintervs_2d_5"))
})

test_that("gintervals.mapply works with INTERVID", {
    expect_regression(gintervals.mapply(function(x) {
        as.numeric(.misha$GAPPLY.INTERVID)
    }, "test.fixedbin", .misha$ALLGENOME), "gintervals.mapply.fixedbin.ALLGENOME.INTERVID")
})

test_that("gintervals.mapply works with intervals.set.out", {
    gintervals.rm("test.testintervs_mapply", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs_mapply", force = TRUE))
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2), 0, -1))
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]
    gintervals.mapply(function(x) {
        max(x + 2, na.rm = TRUE)
    }, "test.fixedbin", intervs, intervals.set.out = "test.testintervs_mapply")
    expect_equal(gintervals.load("test.testintervs_mapply") %>% arrange(chrom, start, end), gintervals.mapply(function(x) {
        max(x + 2, na.rm = TRUE)
    }, "test.fixedbin", intervs) %>% arrange(chrom, start, end))
})

test_that("gintervals.mapply works with intervals.set.out 2d", {
    gintervals.rm("test.testintervs_mapply", force = TRUE)
    withr::defer(gintervals.rm("test.testintervs_mapply", force = TRUE))
    intervs <- gscreen("test.rects > 80", gintervals.2d(1:4))
    set.seed(60427)
    intervs <- intervs[sample(nrow(intervs)), ]
    gintervals.mapply(function(x) {
        max(x + 2, na.rm = TRUE)
    }, "test.rects", intervs, iterator = c(10000, 10000), intervals.set.out = "test.testintervs_mapply")
    expect_equal(
        gintervals.load("test.testintervs_mapply") %>%
            arrange(chrom1, start1, end1, chrom2, start1, end2), gintervals.mapply(function(x) {
            max(x + 2, na.rm = TRUE)
        }, "test.rects", intervs, iterator = c(10000, 10000)) %>% arrange(chrom1, start1, end1, chrom2, start1, end2)
    )
})
