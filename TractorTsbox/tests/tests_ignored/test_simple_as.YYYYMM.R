
testthat::test_that("good result for integer date", {
    testthat::expect_identical(
        object = as_yyyymm(2020L),
        expected = c(2020L, 1L)
    )
})
