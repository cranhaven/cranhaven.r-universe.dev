test_that("gtrack.smooth with test.fixedbin using LINEAR_RAMP", {
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.smooth("test.tmptrack", "", "test.fixedbin", 10000, alg = "LINEAR_RAMP")
    r <- gextract("test.tmptrack", gintervals(c(1, 2), 0, 1000000))
    expect_regression(r, "gtrack.smooth.fixedbin_LINEAR_RAMP")
})

test_that("gtrack.smooth with test.fixedbin using MEAN", {
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.smooth("test.tmptrack", "", "test.fixedbin", 10000, alg = "MEAN")
    r <- gextract("test.tmptrack", gintervals(c(1, 2), 0, 1000000))
    expect_regression(r, "gtrack.smooth.fixedbin_MEAN")
})

test_that("gtrack.smooth with test.sparse using LINEAR_RAMP", {
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.smooth("test.tmptrack", "", "test.sparse", 10000, alg = "LINEAR_RAMP", iterator = 1000)
    r <- gextract("test.tmptrack", gintervals(c(1, 2), 0, 1000000))
    expect_regression(r, "gtrack.smooth.sparse_LINEAR_RAMP")
})

test_that("gtrack.smooth with test.array using LINEAR_RAMP", {
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.smooth("test.tmptrack", "", "test.array", 10000, alg = "LINEAR_RAMP", iterator = 1000)
    r <- gextract("test.tmptrack", gintervals(c(1, 2), 0, 1000000))
    expect_regression(r, "gtrack.smooth.array_LINEAR_RAMP")
})

test_that("gtrack.smooth with test.rects using LINEAR_RAMP", {
    expect_error(gtrack.smooth("test.tmptrack", "", "test.rects", 10000, alg = "LINEAR_RAMP"))
})
