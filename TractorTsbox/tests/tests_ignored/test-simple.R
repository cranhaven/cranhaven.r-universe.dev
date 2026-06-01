
testthat::test_that("good result for integer date", {
    testthat::expect_identical(
        object = libelles_one_date(date_ts = c(2020L, 2L), frequency_ts = 12L),
        expected = "Feb 2020"
    )
})
