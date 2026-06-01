# Initialisation ---------------------------------------------------------------

set.seed(2024L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer date month", {
    for (year in good_years) {
        testthat::expect_silent(assert_date_ts(x = year, frequency_ts = 12L))
        for (month in good_months) {
            testthat::expect_silent(
                assert_date_ts(x = c(year, month), frequency_ts = 12L)
            )
        }
    }
})

testthat::test_that("good result for integer date quarter", {
    for (year in good_years) {
        testthat::expect_silent(assert_date_ts(year, frequency_ts = 4L))
        for (quarter in good_quarters) {
            testthat::expect_silent(
                assert_date_ts(x = c(year, quarter), frequency_ts = 4L)
            )
        }
    }
})


# Tests positifs avec warning --------------------------------------------------

## Mensuel -----------------------------------------------------------------

testthat::test_that("warning for integer date", {
    for (good_year in good_years) {
        for (warning_month in warning_integer_months) {
            warning_date <- c(good_year, warning_month)
            testthat::expect_warning(
                assert_date_ts(x = warning_date, frequency_ts = 12L),
                regexp = invalid_monthly_period
            )
        }
    }
})

testthat::test_that("warning for double date", {
    for (warning_year in double_years) {
        warning_date <- warning_year
        testthat::expect_warning(
            assert_date_ts(x = warning_date, frequency_ts = 12L),
            regexp = message_double("warning_date")
        )

        for (good_month in good_months) {
            warning_date <- c(warning_year, good_month)
            testthat::expect_warning(
                assert_date_ts(x = warning_date, frequency_ts = 12L),
                regexp = message_double("warning_date")
            )
        }
    }

    for (good_year in good_years) {
        for (warning_month in double_months) {
            warning_date <- c(good_year, warning_month)
            testthat::expect_warning(
                assert_date_ts(x = warning_date, frequency_ts = 12L),
                regexp = message_double("warning_date")
            )
        }
    }

    for (warning_year in double_years) {
        for (warning_month in double_months) {
            warning_date <- c(warning_year, warning_month)
            testthat::expect_warning(
                assert_date_ts(x = warning_date, frequency_ts = 12L),
                regexp = message_double("warning_date")
            )
        }
    }
})

testthat::test_that("several warning", {
    for (warning_year in double_years) {
        for (warning_month in warning_integer_months) {
            warning_date <- c(warning_year, warning_month)
            w <- testthat::capture_warnings(
                assert_date_ts(x = warning_date, frequency_ts = 12L)
            )
            testthat::expect_match(
                object = w,
                regexp = message_double("warning_date"),
                all = FALSE
            )
            testthat::expect_match(
                object = w,
                regexp = invalid_monthly_period,
                all = FALSE
            )
        }
    }

    for (warning_year in double_years) {
        for (warning_month in warning_double_months) {
            warning_date <- c(warning_year, warning_month)
            w <- testthat::capture_warnings(
                assert_date_ts(x = warning_date, frequency_ts = 12L)
            )
            testthat::expect_match(
                object = w,
                regexp = message_double("warning_date"),
                all = FALSE
            )
            testthat::expect_match(
                object = w,
                regexp = invalid_monthly_period,
                all = FALSE
            )
        }
    }

    for (good_year in good_years) {
        for (warning_month in warning_double_months) {
            warning_date <- c(warning_year, warning_month)
            w <- testthat::capture_warnings(
                assert_date_ts(x = warning_date, frequency_ts = 12L)
            )
            testthat::expect_match(
                object = w,
                regexp = message_double("warning_date"),
                all = FALSE
            )
            testthat::expect_match(
                object = w,
                regexp = invalid_monthly_period,
                all = FALSE
            )
        }
    }
})


## Trimestriel -----------------------------------------------------------------

testthat::test_that("warning for integer date", {
    for (good_year in good_years) {
        for (warning_quarter in warning_integer_quarters) {
            warning_date <- c(good_year, warning_quarter)
            testthat::expect_warning(
                {
                    boolRes <- assert_date_ts(x = warning_date, frequency_ts = 4L)
                },
                regexp = invalid_quaterly_period
            )
            testthat::expect_silent(boolRes)
        }
    }
})

testthat::test_that("warning for double date", {
    for (warning_year in double_years) {
        warning_date <- warning_year
        testthat::expect_warning(
            {
                boolRes <- assert_date_ts(x = warning_date, frequency_ts = 4L)
            },
            regexp = message_double("warning_date")
        )
        testthat::expect_silent(boolRes)

        for (good_quarter in good_quarters) {
            warning_date <- c(warning_year, good_quarter)
            testthat::expect_warning(
                {
                    boolRes <- assert_date_ts(x = warning_date, frequency_ts = 4L)
                },
                regexp = message_double("warning_date")
            )
            testthat::expect_silent(boolRes)
        }
    }

    for (good_year in good_years) {
        for (warning_quarter in double_quarters) {
            warning_date <- c(good_year, warning_quarter)
            testthat::expect_warning(
                {
                    boolRes <- assert_date_ts(x = warning_date, frequency_ts = 4L)
                },
                regexp = message_double("warning_date")
            )
            testthat::expect_silent(boolRes)
        }
    }

    for (warning_year in double_years) {
        for (warning_quarter in double_quarters) {
            warning_date <- c(warning_year, warning_quarter)
            testthat::expect_warning(
                {
                    boolRes <- assert_date_ts(x = warning_date, frequency_ts = 4L)
                },
                regexp = message_double("warning_date")
            )
            testthat::expect_silent(boolRes)
        }
    }
})

testthat::test_that("several warning", {
    for (warning_year in double_years) {
        for (warning_quarter in warning_integer_quarters) {
            warning_date <- c(warning_year, warning_quarter)
            w <- testthat::capture_warnings({
                boolRes <- assert_date_ts(x = arning_date, frequency_ts = 4L)
            })
            testthat::expect_match(
                object = w,
                regexp = message_double("warning_date"),
                all = FALSE
            )
            testthat::expect_match(
                object = w,
                regexp = invalid_quaterly_period,
                all = FALSE
            )

            testthat::expect_silent(boolRes)
        }
    }

    for (warning_year in double_years) {
        for (warning_quarter in warning_double_quarters) {
            warning_date <- c(warning_year, warning_quarter)
            w <- testthat::capture_warnings({
                boolRes <- assert_date_ts(x = warning_date, frequency_ts = 4L)
            })
            testthat::expect_match(
                object = w,
                regexp = message_double("warning_date"),
                all = FALSE
            )
            testthat::expect_match(
                object = w,
                regexp = invalid_quaterly_period,
                all = FALSE
            )

            testthat::expect_silent(boolRes)
        }
    }

    for (good_year in good_years) {
        for (warning_quarter in warning_double_quarters) {
            warning_date <- c(warning_year, warning_quarter)
            w <- testthat::capture_warnings({
                boolRes <- assert_date_ts(x = warning_date, frequency_ts = 4L)
            })
            testthat::expect_match(
                object = w,
                regexp = message_double("warning_date"),
                all = FALSE
            )
            testthat::expect_match(
                object = w,
                regexp = invalid_quaterly_period,
                all = FALSE
            )

            testthat::expect_silent(boolRes)
        }
    }
})


## Fréquence -------------------------------------------------------------------

testthat::test_that("warning for integer date", {
    for (good_year in good_years) {
        for (good_quarter in good_quarters) {
            good_date <- c(good_year, good_quarter)
            testthat::expect_warning(
                {
                    boolRes <- assert_date_ts(x = good_date, frequency_ts = 4.0)
                },
                regexp = message_double("frequency_ts")
            )
            testthat::expect_silent(boolRes)
        }
    }

    for (good_year in good_years) {
        for (good_month in good_months) {
            good_date <- c(good_year, good_month)
            testthat::expect_warning(
                assert_date_ts(x = good_date, frequency_ts = 12.0),
                regexp = message_double("frequency_ts")
            )
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("detection of wrong dates", {
    for (wrong_date in c(list_wrong_date_ts,
                         object_bank_R[-10L],
                         rnorm(10L))) {
        testthat::expect_error(assert_date_ts(x = wrong_date, frequency_ts = 12L))
        testthat::expect_error(assert_date_ts(x = wrong_date, frequency_ts = 4L))
    }
})

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(
            assert_date_ts(
                x = 2020L,
                frequency_ts = wrong_frequency
            )
        )
    }
})
