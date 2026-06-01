# Initialisation ---------------------------------------------------------------

set.seed(2038L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for Date", {

    list_date <- c(Sys.Date(), create_random_type("Date", len = 10L))

    for (k in seq_along(list_date)) {
        date <- list_date[k]
        res <- testthat::expect_silent(assert_scalar_date(x = date))
        testthat::expect_identical(res, date)
    }

})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("Several Date are not allowed", {

    testthat::expect_error(
        assert_scalar_date(x = seq(from = as.Date("2023-08-04"), to = Sys.Date(), by = "months"))
    )

    for (len in list_len[-2L]) {
        testthat::expect_error(
            assert_scalar_date(x = create_random_type("Date", len = len))
        )
    }

})

testthat::test_that("miscellaneous Object are not allowed", {
    for (wrong_date in object_bank_R[-34L]) {
        testthat::expect_error(
            assert_scalar_date(x = wrong_date)
        )
    }
})
