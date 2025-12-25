testthat::test_that("[32-bit] basic index @ n = 1", {
    expect_equal(hilbert::index(0, 0, n = 1L), 0)
    expect_equal(hilbert::index(0, 1, n = 1L), 1)
    expect_equal(hilbert::index(1, 1, n = 1L), 2)
    expect_equal(hilbert::index(1, 0, n = 1L), 3)
})

testthat::test_that("[32-bit] basic index @ n = 2", {
    expect_equal(hilbert::index(0, 0, n = 2L), 0)
    expect_equal(hilbert::index(0, 1, n = 2L), 3)
    expect_equal(hilbert::index(0, 2, n = 2L), 4)
    expect_equal(hilbert::index(0, 3, n = 2L), 5)

    expect_equal(hilbert::index(1, 0, n = 2L), 1)
    expect_equal(hilbert::index(1, 1, n = 2L), 2)
    expect_equal(hilbert::index(1, 2, n = 2L), 7)
    expect_equal(hilbert::index(1, 3, n = 2L), 6)

    expect_equal(hilbert::index(2, 0, n = 2L), 14)
    expect_equal(hilbert::index(2, 1, n = 2L), 13)
    expect_equal(hilbert::index(2, 2, n = 2L), 8)
    expect_equal(hilbert::index(2, 3, n = 2L), 9)

    expect_equal(hilbert::index(3, 0, n = 2L), 15)
    expect_equal(hilbert::index(3, 1, n = 2L), 12)
    expect_equal(hilbert::index(3, 2, n = 2L), 11)
    expect_equal(hilbert::index(3, 3, n = 2L), 10)
})

testthat::test_that("[64-bit] basic index @ n = 1", {
    testthat::skip_if_not_installed("bit64")

    expect_equal(hilbert::index64(0, 0, n = 1L), bit64::as.integer64(0))
    expect_equal(hilbert::index64(0, 1, n = 1L), bit64::as.integer64(1))
    expect_equal(hilbert::index64(1, 1, n = 1L), bit64::as.integer64(2))
    expect_equal(hilbert::index64(1, 0, n = 1L), bit64::as.integer64(3))
})

testthat::test_that("[64-bit] basic index @ n = 2", {
    testthat::skip_if_not_installed("bit64")

    expect_equal(hilbert::index64(0, 0, n = 2L), bit64::as.integer64(0))
    expect_equal(hilbert::index64(0, 1, n = 2L), bit64::as.integer64(3))
    expect_equal(hilbert::index64(0, 2, n = 2L), bit64::as.integer64(4))
    expect_equal(hilbert::index64(0, 3, n = 2L), bit64::as.integer64(5))

    expect_equal(hilbert::index64(1, 0, n = 2L), bit64::as.integer64(1))
    expect_equal(hilbert::index64(1, 1, n = 2L), bit64::as.integer64(2))
    expect_equal(hilbert::index64(1, 2, n = 2L), bit64::as.integer64(7))
    expect_equal(hilbert::index64(1, 3, n = 2L), bit64::as.integer64(6))

    expect_equal(hilbert::index64(2, 0, n = 2L), bit64::as.integer64(14))
    expect_equal(hilbert::index64(2, 1, n = 2L), bit64::as.integer64(13))
    expect_equal(hilbert::index64(2, 2, n = 2L), bit64::as.integer64(8))
    expect_equal(hilbert::index64(2, 3, n = 2L), bit64::as.integer64(9))

    expect_equal(hilbert::index64(3, 0, n = 2L), bit64::as.integer64(15))
    expect_equal(hilbert::index64(3, 1, n = 2L), bit64::as.integer64(12))
    expect_equal(hilbert::index64(3, 2, n = 2L), bit64::as.integer64(11))
    expect_equal(hilbert::index64(3, 3, n = 2L), bit64::as.integer64(10))
})