test_that("test for aaaaaaaaaaaaa.bbbbbbbbbbb", {
    withr::defer(gtrack.rm("aaaaaaaaaaaaa.bbbbbbbbbbb", force = TRUE))
    expect_error(gtrack.create("aaaaaaaaaaaaa.bbbbbbbbbbb", "", "test.fixedbin"))
    gdir.create("aaaaaaaaaaaaa")
    withr::defer(gdir.rm("aaaaaaaaaaaaa", recursive = TRUE, force = TRUE))
    gtrack.create("aaaaaaaaaaaaa.bbbbbbbbbbb", "", "test.fixedbin")
    r <- gextract("aaaaaaaaaaaaa.bbbbbbbbbbb", gintervals(c(1, 2), 0, 1000000))
    expect_regression(r, "gtrack.create.1")
})

test_that("test for test.tmptrack fixedbin+1", {
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.create("test.tmptrack", "", "test.fixedbin+1")
    r <- gextract("test.tmptrack", gintervals(c(1, 2), 0, 1000000))
    expect_regression(r, "gtrack.create.2")
})

test_that("test for test.tmptrack with sparse iterator", {
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.create("test.tmptrack", "", "test.fixedbin+1", iterator = "test.sparse")
    r <- gextract("test.tmptrack", gintervals(c(1, 2), 0, 1000000))
    expect_regression(r, "gtrack.create.3")
})

test_that("test for test.tmptrack with array iterator", {
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.create("test.tmptrack", "", "test.fixedbin+1", iterator = "test.array")
    r <- gextract("test.tmptrack", gintervals(c(1, 2), 0, 1000000))
    expect_regression(r, "gtrack.create.4")
})

test_that("test for test.tmptrack rects+10", {
    gtrack.rm("test.tmptrack", force = TRUE)
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.create("test.tmptrack", "", "test.rects+10")
    r <- gextract("test.tmptrack", gintervals.2d(chroms1 = c(2, 3), chroms2 = c(2, 4)))
    expect_regression(r, "gtrack.create.5")
})

test_that("test for test.tmptrack with sparse intervals", {
    gtrack.rm("test.tmptrack", force = TRUE)
    intervs <- giterator.intervals("test.sparse", gintervals(c(1, 3, 4)))
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.create("test.tmptrack", "", "test.fixedbin+1", iterator = intervs)
    r <- gextract("test.tmptrack", gintervals(c(1, 2), 0, 1000000))
    expect_regression(r, "gtrack.create.6")
})

test_that("test for test.tmptrack with array intervals", {
    gtrack.rm("test.tmptrack", force = TRUE)
    intervs <- giterator.intervals("test.array", gintervals(c(1, 3, 4)))
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.create("test.tmptrack", "", "test.fixedbin+1", iterator = intervs)
    r <- gextract("test.tmptrack", gintervals(c(1, 2), 0, 1000000))
    expect_regression(r, "gtrack.create.7")
})

test_that("test for test.tmptrack with rects intervals", {
    gtrack.rm("test.tmptrack", force = TRUE)
    intervs <- giterator.intervals("test.rects", gintervals.2d(chroms1 = c(2, 3, 5), chroms2 = c(2, 4, 7)))
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.create("test.tmptrack", "", "test.rects+10", iterator = intervs)
    r <- gextract("test.tmptrack", gintervals.2d(chroms1 = c(2, 3, 3), chroms2 = c(2, 3, 4)))
    expect_regression(r, "gtrack.create.8")
})

test_that("test for sparse test.tmptrack with fixedbin", {
    gtrack.rm("test.tmptrack", force = TRUE)
    intervs <- gscreen("test.fixedbin > 0.2", gintervals(c(1, 2)))
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.create_sparse("test.tmptrack", "", intervs, 1:dim(intervs)[1])
    r <- gextract("test.tmptrack", gintervals(c(1, 2), 0, 1000000))
    expect_regression(r, "gtrack.create.9")
})

test_that("test for sparse test.tmptrack 2d", {
    expect_error(gtrack.create_sparse("test.tmptrack", "", gintervals.2d(c(1, 2)), 1:2))
})

test_that("test for 2d test.tmptrackv", {
    gtrack.rm("test.tmptrack", force = TRUE)
    intervs <- gscreen("test.rects > 80", gintervals.2d(c(1, 2)))
    withr::defer(gtrack.rm("test.tmptrack", force = TRUE))
    gtrack.2d.create("test.tmptrack", "", intervs, 1:dim(intervs)[1])
    r <- gextract("test.tmptrack", gintervals.2d(c(1, 2, 3)))
    expect_regression(r, "gtrack.create.11")
})

test_that("test for 2d test.tmptrack", {
    intervs <- gintervals(c(1, 2))
    expect_error(gtrack.2d.create("test.tmptrack", "", intervs, 1:dim(intervs)[1]))
})
