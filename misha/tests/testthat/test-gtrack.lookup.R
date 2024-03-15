test_that("lookup and extract with default binning", {
    m1 <- matrix(1:15, nrow = 5, ncol = 3)
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.lookup("test.tmptrack", "", m1, "test.fixedbin", seq(0.1, 0.2, length.out = 6), "test.sparse", seq(0.25, 0.48, length.out = 4), iterator = "test.fixedbin")
    r <- gextract("test.tmptrack", gintervals(c(1, 3)))
    expect_regression(r, "gtrack.lookup.default_binning")
})

test_that("lookup and extract without force binning", {
    m1 <- matrix(1:15, nrow = 5, ncol = 3)
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.lookup("test.tmptrack", "", m1, "test.fixedbin", seq(0.1, 0.2, length.out = 6), "test.sparse", seq(0.25, 0.48, length.out = 4), force.binning = FALSE, iterator = "test.fixedbin")
    r <- gextract("test.tmptrack", gintervals(c(1, 3)))
    expect_regression(r, "gtrack.lookup.no_force_binning")
})

test_that("lookup with rects and extract 2D intervals", {
    m1 <- matrix(1:15, nrow = 5, ncol = 3)
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.lookup("test.tmptrack", "", m1, "test.rects", seq(50, 100, length.out = 6), "test.rects / 2", seq(0, 40, length.out = 4), force.binning = FALSE)
    r <- gextract("test.tmptrack", gintervals.2d(chroms1 = c(3, 5), chroms2 = c(4, 6)))
    expect_regression(r, "gtrack.lookup.2D_intervals")
})
