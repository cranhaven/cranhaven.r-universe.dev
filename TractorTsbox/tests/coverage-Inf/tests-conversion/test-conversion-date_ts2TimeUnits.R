# Initialisation ---------------------------------------------------------------

set.seed(2031L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer date", {
    for (year in good_years) {
        testthat::expect_equal(
            date_ts2timeunits(date_ts = year, frequency_ts = 12L),
            year
        )
        for (month in good_months) {
            testthat::expect_equal(
                date_ts2timeunits(date_ts = c(year, month), frequency_ts = 12L),
                year + (month - 1) / 12
            )
        }
    }
})

testthat::test_that("good result for integer date", {
    for (year in good_years) {
        testthat::expect_equal(
            date_ts2timeunits(date_ts = year, frequency_ts = 4L),
            year
        )
        for (quarter in good_quarters) {
            testthat::expect_equal(
                date_ts2timeunits(date_ts = c(year, quarter), frequency_ts = 4L),
                year + (quarter - 1) / 4
            )
        }
    }
})


# Test de résultat positif avec warnings ----------------------------------

testthat::test_that("warning for integer date", {
    for (year in good_years) {
        for (month in warning_integer_months) {
            testthat::expect_warning(
                {
                    resTU <- date_ts2timeunits(date_ts = c(year, month), frequency_ts = 12L)
                },
                regexp = invalid_monthly_period
            )
            testthat::expect_equal(resTU, year + (month - 1) / 12)
        }
    }
})


testthat::test_that("good result for integer date", {
    for (year in good_years) {
        for (quarter in warning_integer_quarters) {
            testthat::expect_warning(
                {
                    resTU <- date_ts2timeunits(date_ts = c(year, quarter), frequency_ts = 4L)
                },
                regexp = invalid_quaterly_period
            )
            testthat::expect_equal(resTU, year + (quarter - 1) / 4)
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (wrong_date in list_wrong_date_ts) {
        testthat::expect_error(date_ts2timeunits(date_ts = wrong_date, frequency_ts = 12L))
    }
    for (wrong_date in list_wrong_date_ts) {
        testthat::expect_error(date_ts2timeunits(date_ts = wrong_date, frequency_ts = 4L))
    }
})

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(date_ts2timeunits(date_ts = create_random_date_ts(), frequency_ts = wrong_frequency))
    }
})
