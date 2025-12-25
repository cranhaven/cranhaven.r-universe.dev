testthat::test_that("[32-bit] basic postion @ n = 1", {
    expect_equal(hilbert::position(0, n = 1L), data.frame(x = 0, y = 0))
    expect_equal(hilbert::position(1, n = 1L), data.frame(x = 0, y = 1))
    expect_equal(hilbert::position(2, n = 1L), data.frame(x = 1, y = 1))
    expect_equal(hilbert::position(3, n = 1L), data.frame(x = 1, y = 0))

    expect_equal(
        hilbert::position(
            data.frame(
                some = 1:3,
                fake = 1:3,
                index = 1:3
            ),
            idx = 3L,
            n = 1L
        ),
        data.frame(
            some  = 1:3,
            fake  = 1:3,
            index = 1:3,
            x     = c(0, 1, 1),
            y     = c(1, 1, 0)
        )
    )

    expect_equal(
        hilbert::position(
            data.frame(
                some = 1:3,
                fake = 1:3,
                index = 1:3
            ),
            idx = 3L,
            n = 1L,
            attach = FALSE
        ),
        data.frame(
            x     = c(0, 1, 1),
            y     = c(1, 1, 0)
        )
    )
})

testthat::test_that("[32-bit] basic position @ n = 2", {
    expect_equal(hilbert::position(0,  n = 2L), data.frame(x = 0, y = 0))
    expect_equal(hilbert::position(3,  n = 2L), data.frame(x = 0, y = 1))
    expect_equal(hilbert::position(4,  n = 2L), data.frame(x = 0, y = 2))
    expect_equal(hilbert::position(5,  n = 2L), data.frame(x = 0, y = 3))

    expect_equal(hilbert::position(1,  n = 2L), data.frame(x = 1, y = 0))
    expect_equal(hilbert::position(2,  n = 2L), data.frame(x = 1, y = 1))
    expect_equal(hilbert::position(7,  n = 2L), data.frame(x = 1, y = 2))
    expect_equal(hilbert::position(6,  n = 2L), data.frame(x = 1, y = 3))

    expect_equal(hilbert::position(14, n = 2L), data.frame(x = 2, y = 0))
    expect_equal(hilbert::position(13, n = 2L), data.frame(x = 2, y = 1))
    expect_equal(hilbert::position(8,  n = 2L), data.frame(x = 2, y = 2))
    expect_equal(hilbert::position(9,  n = 2L), data.frame(x = 2, y = 3))

    expect_equal(hilbert::position(15, n = 2L), data.frame(x = 3, y = 0))
    expect_equal(hilbert::position(12, n = 2L), data.frame(x = 3, y = 1))
    expect_equal(hilbert::position(11, n = 2L), data.frame(x = 3, y = 2))
    expect_equal(hilbert::position(10, n = 2L), data.frame(x = 3, y = 3))
})

testthat::test_that("[64-bit] basic postion @ n = 1", {
    testthat::skip_if_not_installed("bit64")

    expect_equal(
        hilbert::position64(0, n = 1L),
        data.frame(x = bit64::as.integer64(0), y = bit64::as.integer64(0))
    )

    expect_equal(
        hilbert::position64(1, n = 1L),
        data.frame(x = bit64::as.integer64(0), y = bit64::as.integer64(1))
    )

    expect_equal(
        hilbert::position64(2, n = 1L),
        data.frame(x = bit64::as.integer64(1), y = bit64::as.integer64(1))
    )

    expect_equal(
        hilbert::position64(3, n = 1L),
        data.frame(x = bit64::as.integer64(1), y = bit64::as.integer64(0))
    )

})

testthat::test_that("[64-bit] basic position @ n = 2", {
    testthat::skip_if_not_installed("bit64")

    expect_equal(
        hilbert::position64(0,  n = 2L),
        data.frame(x = bit64::as.integer64(0), y = bit64::as.integer64(0))
    )

    expect_equal(
        hilbert::position64(3,  n = 2L),
        data.frame(x = bit64::as.integer64(0), y = bit64::as.integer64(1))
    )

    expect_equal(
        hilbert::position64(4,  n = 2L),
        data.frame(x = bit64::as.integer64(0), y = bit64::as.integer64(2))
    )

    expect_equal(
        hilbert::position64(5,  n = 2L),
        data.frame(x = bit64::as.integer64(0), y = bit64::as.integer64(3))
    )

    expect_equal(
        hilbert::position64(1,  n = 2L),
        data.frame(x = bit64::as.integer64(1), y = bit64::as.integer64(0))
    )

    expect_equal(
        hilbert::position64(2,  n = 2L),
        data.frame(x = bit64::as.integer64(1), y = bit64::as.integer64(1))
    )

    expect_equal(
        hilbert::position64(7,  n = 2L),
        data.frame(x = bit64::as.integer64(1), y = bit64::as.integer64(2))
    )

    expect_equal(
        hilbert::position64(6,  n = 2L),
        data.frame(x = bit64::as.integer64(1), y = bit64::as.integer64(3))
    )

    expect_equal(
        hilbert::position64(14, n = 2L),
        data.frame(x = bit64::as.integer64(2), y = bit64::as.integer64(0))
    )

    expect_equal(
        hilbert::position64(13, n = 2L),
        data.frame(x = bit64::as.integer64(2), y = bit64::as.integer64(1))
    )

    expect_equal(
        hilbert::position64(8,  n = 2L),
        data.frame(x = bit64::as.integer64(2), y = bit64::as.integer64(2))
    )

    expect_equal(
        hilbert::position64(9,  n = 2L),
        data.frame(x = bit64::as.integer64(2), y = bit64::as.integer64(3))
    )

    expect_equal(
        hilbert::position64(15, n = 2L),
        data.frame(x = bit64::as.integer64(3), y = bit64::as.integer64(0))
    )

    expect_equal(
        hilbert::position64(12, n = 2L),
        data.frame(x = bit64::as.integer64(3), y = bit64::as.integer64(1))
    )

    expect_equal(
        hilbert::position64(11, n = 2L),
        data.frame(x = bit64::as.integer64(3), y = bit64::as.integer64(2))
    )

    expect_equal(
        hilbert::position64(10, n = 2L),
        data.frame(x = bit64::as.integer64(3), y = bit64::as.integer64(3))
    )

})