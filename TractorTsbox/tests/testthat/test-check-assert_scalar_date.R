
# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for Date", {

    date <- Sys.Date()
    res <- testthat::expect_silent(assert_scalar_date(x = date))
    testthat::expect_identical(res, date)

})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("Several Date are not allowed", {

    testthat::expect_error(
        assert_scalar_date(x = seq(from = as.Date("2023-08-04"), to = as.Date("2024-08-04"), by = "months"))
    )

})
