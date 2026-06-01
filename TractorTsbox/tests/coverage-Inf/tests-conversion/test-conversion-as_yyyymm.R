# Initialisation ---------------------------------------------------------------

set.seed(2028L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer date", {
    for (month in c(warning_integer_months, good_months)) {
        for (year in good_years) {
            TSdate <- as.integer(c(year + month %/% 12L, month %% 12L + 1L))
            res <- as_yyyymm(year + month / 12L)
            testthat::expect_type(res, "integer")
            testthat::expect_identical(res, TSdate)
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous input are not allowed", {
    for (wrong_time in object_bank_R[-c(10L, 16L)]) {
        testthat::expect_error(as_yyyymm(wrong_time))
    }
    for (wrong_time in list_wrong_timeunits) {
        testthat::expect_error(as_yyyymm(wrong_time))
    }
})
