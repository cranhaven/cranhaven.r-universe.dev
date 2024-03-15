test_that("gcis_decay works", {
    domain <- rbind(
        gintervals(1, 800000 * (0:5), 800000 * (0:5) + 400000),
        gintervals(2, 800000 * (0:5), 800000 * (0:5) + 400000),
        gintervals(3, 800000 * (0:5), 800000 * (0:5) + 400000),
        gintervals(4, 800000 * (0:5), 800000 * (0:5) + 400000),
        gintervals(5, 800000 * (0:5), 800000 * (0:5) + 400000)
    )
    src <- gextract("test.sparse", gintervals(c(1, 2, 3, 4, 5)))
    expect_regression(gcis_decay("test.rects", (0:20) * 1000, src, domain), "gcis_decay.1")
    src <- domain
    expect_regression(gcis_decay("test.rects", (0:20) * 1000, src, domain), "gcis_decay.2")
})

test_that("gcompute_strands_autocorr works", {
    expect_regression(gcompute_strands_autocorr(file.path(.misha$GROOT, "s_7_export.txt"), 1, 50), "gcompute_strands_autocorr.1")
})
