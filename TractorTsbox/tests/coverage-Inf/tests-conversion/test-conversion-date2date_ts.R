# Initialisation ---------------------------------------------------------------

set.seed(2045L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result with Date", {

    testthat::expect_silent(date2date_ts(date = Sys.Date(), frequency_ts = 4L))
    testthat::expect_silent(date2date_ts(date = Sys.Date(), frequency_ts = 12L))

    for (year in good_years[-1:-2]) {
        for (month in good_months) {
            for (day in seq_len(28L)) {
                dateA <- as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")

                testthat::expect_identical(
                    object = date2date_ts(date = dateA, frequency_ts = 12L),
                    expected = c(year, month)
                )
                testthat::expect_identical(
                    object = date2date_ts(date = dateA, frequency_ts = 4L),
                    expected = c(year, (month + 2L) %/% 3L)
                )
            }
        }
    }
})


# Test de résultat positif avec warnings ---------------------------------------

## Fréquence -------------------------------------------------------------------

testthat::test_that("warning for double frequency", {

    testthat::expect_warning(
        {res <- date2date_ts(date = Sys.Date(), frequency_ts = 12.0)},
        regexp = message_double("frequency_ts")
    )

    testthat::expect_warning(
        {res <- date2date_ts(date = Sys.Date(), frequency_ts = 4.0)},
        regexp = message_double("frequency_ts")
    )

    for (year in good_years[-1:-2]) {
        for (month in good_months) {
            for (day in seq_len(28L)) {
                dateA <- as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")

                testthat::expect_warning(
                    {res <- date2date_ts(date = dateA, frequency_ts = 12.0)},
                    regexp = message_double("frequency_ts")
                )
                testthat::expect_identical(res, c(year, month))

                testthat::expect_warning(
                    {res <- date2date_ts(date = dateA, frequency_ts = 4.0)},
                    regexp = message_double("frequency_ts")
                )
                testthat::expect_identical(res, c(year, (month + 2L) %/% 3L))
            }

        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (wrong_date in object_bank_R[-34L]) {
        testthat::expect_error(date2date_ts(date = wrong_date, frequency_ts = 12L))
    }
    for (wrong_date in object_bank_R[-34L]) {
        testthat::expect_error(date2date_ts(date = wrong_date, frequency_ts = 4L))
    }
})

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(date2date_ts(date = create_random_type(type = "Date", len = 1L), frequency_ts = wrong_frequency))
    }
})
