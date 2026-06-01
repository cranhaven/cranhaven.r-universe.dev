# Initialisation ---------------------------------------------------------------

set.seed(2034L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer date month", {
    for (year in good_years) {
        res <- testthat::expect_silent(assert_timeunits(x = year, frequency_ts = 12L))
        testthat::expect_identical(res, year)
        for (month in good_months) {
            good_timeunits <- year + (month - 1) / 12
            res <- testthat::expect_silent(assert_timeunits(x = good_timeunits, frequency_ts = 12L))
            testthat::expect_identical(res, good_timeunits)
        }
    }
})

testthat::test_that("good result for integer date quarter", {
    for (year in good_years) {
        res <- testthat::expect_silent(assert_timeunits(x = year, frequency_ts = 4L))
        testthat::expect_identical(res, year)
        for (quarter in good_quarters) {
            good_timeunits <- year + (quarter - 1) / 4
            res <- testthat::expect_silent(assert_timeunits(x = good_timeunits, frequency_ts = 4L))
            testthat::expect_identical(res, good_timeunits)
        }
    }
})


# Tests positifs avec warning --------------------------------------------------

## Fréquence -------------------------------------------------------------------

testthat::test_that("warning for integer date", {
    for (year in good_years) {
        for (month in good_months) {
            good_timeunits <- year + (month - 1L) / 12L
            testthat::expect_warning(
                {
                    res <- assert_timeunits(x = good_timeunits, frequency_ts = 12.0)
                },
                regexp = message_double("frequency_ts")
            )
            testthat::expect_identical(res, good_timeunits)
        }
    }

    for (year in good_years) {
        for (quarter in good_quarters) {
            good_timeunits <- year + (quarter - 1L) / 4L
            testthat::expect_warning(
                {
                    res <- assert_timeunits(x = good_timeunits, frequency_ts = 4.0)
                },
                regexp = message_double("frequency_ts")
            )
            testthat::expect_identical(res, good_timeunits)
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("detection of wrong object", {
    for (wrong_timeunits in c(object_bank_R[-c(10L, 16L)], list_wrong_timeunits)) {
        testthat::expect_error(assert_timeunits(x = wrong_timeunits, frequency_ts = 12L))
        testthat::expect_error(assert_timeunits(x = wrong_timeunits, frequency_ts = 4L))
    }
})


testthat::test_that("detection of wrong timesunits in quarterly", {
    for (year in good_years) {
        for (month in setdiff(good_months, (1:4) * 3) + 1) {
            wrong_timeunits <- year + (month - 1) / 12
            testthat::expect_error(assert_timeunits(x = wrong_timeunits, frequency_ts = 4L))
        }
    }
})

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(
            assert_timeunits(
                x = create_random_date_ts(),
                frequency_ts = wrong_frequency
            )
        )
    }
})
