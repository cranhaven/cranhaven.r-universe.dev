
testthat::test_that("good result for integer date", {
    testthat::expect_identical(
        object = as_yyyymm(2020L),
        expected = c(2020L, 1L)
    )
    testthat::expect_identical(
        object = as_yyyymm(2020L),
        expected = c(2020L, 2L)
    )
})

testthat::test_that("good result for integer freq", {
    testthat::expect_identical(
        object = as_yyyymm(2020L),
        expected = c(2020L, 1L)
    )
    testthat::expect_identical(
        object = libelles_one_date(2020L, 12),
        expected = "Jan 2020"
    )
})
