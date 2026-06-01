# Initialisation ---------------------------------------------------------------

set.seed(2027L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer date", {
    for (quarter in c(warning_integer_quarters, good_quarters)) {
        for (year in good_years) {
            TSdate <- as.integer(c(year + quarter %/% 4L, quarter %% 4L + 1L))
            res <- as_yyyytt(year + quarter / 4L)
            testthat::expect_type(res, "integer")
            testthat::expect_identical(res, TSdate)
        }
    }
})

# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous input are not allowed", {
    for (wrong_time in object_bank_R[-c(10L, 16L)]) {
        testthat::expect_error(as_yyyytt(wrong_time))
    }
    for (wrong_time in list_wrong_timeunits) {
        testthat::expect_error(as_yyyytt(wrong_time))
    }
})
