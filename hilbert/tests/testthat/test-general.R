x <- c(-77.85641, -77.86663, -77.85641, -77.86358, -77.86372, -77.86627)
y <- c(34.35935, 34.36440, 34.35936, 34.36328, 34.36336, 34.35290)
world_extent <- c(xmax = 180, xmin = -180, ymax = 90, ymin = -90)
end <- if (requireNamespace("bit64", quietly = TRUE)) 31 else 15

testthat::test_that("coords -> position -> index -> position", {
    for (n in seq_len(end)) {
        test_xy       <- coords_to_position(x, y, n = n, extent = world_extent)
        test_index    <- index(test_xy$x, test_xy$y, n = n)
        test_position <- position(test_index, n = n)

        expect_equal(test_position, test_xy)
    }
})

testthat::test_that("coordinates <--> position", {
    for (n in 6:end) {
        test_xy     <- coords_to_position(x, y, n = n, extent = world_extent)
        test_coords <- position_to_coords(test_xy$x, test_xy$y, n = n,
                                          extent = world_extent)

        expect_equal(test_coords, data.frame(x = x, y = y), tolerance = 0.1)
    }
})

testthat::test_that("coordinates are converted to correct position", {
    pos_x    <- c(7, 0, 7, 2, 2, 0)
    pos_y    <- c(4, 7, 4, 7, 7, 0)
    test_pos <- hilbert::coords_to_position(x = x, y = y, n = 3L,
                                           extent = .extent(x, y))

    expect_equal(test_pos, data.frame(x = pos_x, y = pos_y))
})