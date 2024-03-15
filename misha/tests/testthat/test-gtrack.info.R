test_that("gtrack.info works", {
    expect_equal(
        gtrack.info("test.fixedbin"),
        list(
            type = "dense", dimensions = 1L, size.in.bytes = 246433708,
            bin.size = 50L
        )
    )
    expect_equal(
        gtrack.info("test.sparse"),
        list(type = "sparse", dimensions = 1L, size.in.bytes = 37365316)
    )
    expect_equal(
        gtrack.info("test.array"),
        list(type = "array", dimensions = 1L, size.in.bytes = 164405812)
    )
    expect_equal(
        gtrack.info("test.rects"),
        list(type = "rectangles", dimensions = 2L, size.in.bytes = 214645968)
    )
    expect_equal(
        gtrack.info("test.computed2d"),
        list(type = "computed", dimensions = 2L, size.in.bytes = 496780680)
    )
})
