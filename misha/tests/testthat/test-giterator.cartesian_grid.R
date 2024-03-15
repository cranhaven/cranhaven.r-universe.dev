test_that("gterator.cartesian_grid works (1)", {
    intervs1 <- gscreen("test.sparse>1.5 & test.sparse<1.6", gintervals(c(1, 2, 3)))
    intervs2 <- gscreen("test.sparse>1.55", gintervals(c(1, 2, 3)))
    itr <- giterator.cartesian_grid(intervs1, c(-100000, -50000, -10000, 20000, 700000), intervs2, c(-200000, -30000, -10000, 60000, 100000, 200000))
    expect_regression(giterator.intervals("1", .misha$ALLGENOME, iterator = itr), "giterator.cartesian_grid.1")
})

test_that("gterator.cartesian_grid works with band", {
    intervs1 <- gscreen("test.sparse>1 & test.sparse<1.2", gintervals(c(1, 2, 3)))
    intervs2 <- gscreen("test.sparse>1.1", gintervals(c(1, 2, 3)))
    itr <- giterator.cartesian_grid(intervs1, c(-100000, -50000, -10000, 20000, 700000), intervs2, c(-200000, -30000, -10000, 60000, 100000, 200000))
    expect_regression(giterator.intervals("1", .misha$ALLGENOME, iterator = itr, band = c(-20000, 30000)), "giterator.cartesian_grid.band")
    expect_error(giterator.intervals("1", .misha$ALLGENOME, iterator = itr, band = c(1, 1)))
})

test_that("gterator.cartesian_grid works with band (1d)", {
    intervs1 <- gscreen("test.sparse>1 & test.sparse<1.2", gintervals(c(1, 2, 3)))
    itr <- giterator.cartesian_grid(intervs1, c(-100000, -50000, -10000, 20000, 700000))
    expect_regression(giterator.intervals("1", .misha$ALLGENOME, iterator = itr, band = c(-20000, 30000)), "giterator.cartesian_grid.band.1d")
})

test_that("gterator.cartesian_grid works with min.band.idx", {
    intervs1 <- gscreen("test.sparse>1 & test.sparse<1.2", gintervals(c(1, 2, 3)))
    expect_error(giterator.cartesian_grid(intervs1, c(-100000, -50000, -10000, 20000, 700000), gintervals(1, 100, 300), min.band.idx = -1, max.band.idx = 2))
    itr <- giterator.cartesian_grid(intervs1, c(-100000, -50000, -10000, 20000, 700000), min.band.idx = -1, max.band.idx = 2)
    expect_regression(giterator.intervals("1", .misha$ALLGENOME, iterator = itr), "giterator.cartesian_grid.min.band.idx")
    expect_regression(giterator.intervals("1", "test.generated_2d_5", iterator = itr), "giterator.cartesian_grid.min.band.idx.2")
    expect_regression(giterator.intervals("1", .misha$ALLGENOME, iterator = itr, band = c(-20000, 30000)), "giterator.cartesian_grid.min.band.idx.3")
})
