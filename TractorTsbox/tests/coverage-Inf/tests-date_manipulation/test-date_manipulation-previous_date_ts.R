# Initialisation ---------------------------------------------------------------

set.seed(2042L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that(desc = "Expect good result with monthly frequency", {

    for (year in good_years) {
        for (lag in list_lag) {

            date_a <- year

            minus_year <- (-lag) %/% 12L
            minus_months <- (-lag) %% 12L + 1L

            date_b <- c(year + minus_year, minus_months)

            testthat::expect_identical(
                object = previous_date_ts(date_a, frequency_ts = 12L, lag = lag),
                expected = date_b
            )


            for (month in good_months) {
                date_a <- c(year, month)

                minus_year <- (month - lag - 1L) %/% 12L
                minus_months <- (month - lag - 1L) %% 12L + 1L

                date_b <- c(year + minus_year, minus_months)

                testthat::expect_identical(
                    object = previous_date_ts(date_a, frequency_ts = 12L, lag = lag),
                    expected = date_b
                )
            }
        }
    }
})

testthat::test_that(desc = "Expect good result with quaterly frequency", {

    for (year in good_years) {
        for (lag in list_lag) {

            date_a <- year

            minus_year <- (-lag) %/% 4L
            minus_months <- (-lag) %% 4L + 1L

            date_b <- c(year + minus_year, minus_months)

            testthat::expect_identical(
                object = previous_date_ts(date_a, frequency_ts = 4L, lag = lag),
                expected = date_b
            )


            for (quarter in good_quarters) {
                date_a <- c(year, quarter)

                minus_year <- (quarter - lag - 1L) %/% 4L
                minus_quarters <- (quarter - lag - 1L) %% 4L + 1L

                date_b <- c(year + minus_year, minus_quarters)

                testthat::expect_identical(
                    object = previous_date_ts(date_a, frequency_ts = 4L, lag = lag),
                    expected = date_b
                )
            }
        }
    }
})


# Tests de résultats positif avec warning --------------------------------------

testthat::test_that(desc = "Warning with double years and monthly frequency", {

    for (year in double_years) {
        for (lag in list_lag) {

            date_a <- year

            minus_year <- (-lag) %/% 12L
            minus_months <- (-lag) %% 12L + 1L

            date_b <- c(year + minus_year, minus_months) |> as.integer()

            testthat::expect_warning(
                object = {
                    res <- previous_date_ts(date_a, frequency_ts = 12L, lag = lag)
                },
                regexp = message_double("date_ts")
            )

            testthat::expect_identical(
                object = res,
                expected = date_b
            )


            for (month in good_months) {
                date_a <- c(year, month)

                minus_year <- (month - lag - 1L) %/% 12L
                minus_months <- (month - lag - 1L) %% 12L + 1L

                date_b <- c(year + minus_year, minus_months) |> as.integer()

                testthat::expect_warning(
                    object = {
                        res <- previous_date_ts(date_a, frequency_ts = 12L, lag = lag)
                    },
                    regexp = message_double("date_ts")
                )

                testthat::expect_identical(
                    object = res,
                    expected = date_b
                )
            }
        }
    }
})

testthat::test_that(desc = "Warning with double years and quaterly frequency", {

    for (year in double_years) {
        for (lag in list_lag) {

            date_a <- year

            minus_year <- (-lag) %/% 4L
            minus_months <- (-lag) %% 4L + 1L

            date_b <- c(year + minus_year, minus_months) |> as.integer()

            testthat::expect_warning(
                object = {
                    res <- previous_date_ts(date_a, frequency_ts = 4L, lag = lag)
                },
                regexp = message_double("date_ts")
            )

            testthat::expect_identical(
                object = res,
                expected = date_b
            )


            for (quarter in good_quarters) {
                date_a <- c(year, quarter)

                minus_year <- (quarter - lag - 1L) %/% 4L
                minus_quarters <- (quarter - lag - 1L) %% 4L + 1L

                date_b <- c(year + minus_year, minus_quarters) |> as.integer()

                testthat::expect_warning(
                    object = {
                        res <- previous_date_ts(date_a, frequency_ts = 4L, lag = lag)
                    },
                    regexp = message_double("date_ts")
                )

                testthat::expect_identical(
                    object = res,
                    expected = date_b
                )
            }
        }
    }
})

testthat::test_that(desc = "Warning with frequency and monthly frequency", {

    for (year in good_years) {
        for (lag in list_lag) {

            date_a <- year

            minus_year <- (-lag) %/% 12L
            minus_months <- (-lag) %% 12L + 1L

            date_b <- c(year + minus_year, minus_months) |> as.integer()

            testthat::expect_warning(
                object = {
                    res <- previous_date_ts(date_a, frequency_ts = 12, lag = lag)
                },
                regexp = message_double("frequency_ts")
            )

            testthat::expect_identical(
                object = res,
                expected = date_b
            )


            for (month in good_months) {
                date_a <- c(year, month)

                minus_year <- (month - lag - 1L) %/% 12L
                minus_months <- (month - lag - 1L) %% 12L + 1L

                date_b <- c(year + minus_year, minus_months) |> as.integer()

                testthat::expect_warning(
                    object = {
                        res <- previous_date_ts(date_a, frequency_ts = 12, lag = lag)
                    },
                    regexp = message_double("frequency_ts")
                )

                testthat::expect_identical(
                    object = res,
                    expected = date_b
                )
            }
        }
    }
})

testthat::test_that(desc = "Warning with frequency and quaterly frequency", {

    for (year in good_years) {
        for (lag in list_lag) {

            date_a <- year

            minus_year <- (-lag) %/% 4L
            minus_months <- (-lag) %% 4L + 1L

            date_b <- c(year + minus_year, minus_months) |> as.integer()

            testthat::expect_warning(
                object = {
                    res <- previous_date_ts(date_a, frequency_ts = 4, lag = lag)
                },
                regexp = message_double("frequency_ts")
            )

            testthat::expect_identical(
                object = res,
                expected = date_b
            )


            for (quarter in good_quarters) {
                date_a <- c(year, quarter)

                minus_year <- (quarter - lag - 1L) %/% 4L
                minus_quarters <- (quarter - lag - 1L) %% 4L + 1L

                date_b <- c(year + minus_year, minus_quarters) |> as.integer()

                testthat::expect_warning(
                    object = {
                        res <- previous_date_ts(date_a, frequency_ts = 4, lag = lag)
                    },
                    regexp = message_double("frequency_ts")
                )

                testthat::expect_identical(
                    object = res,
                    expected = date_b
                )
            }
        }
    }
})

# Tests de résultats négatifs (erreur) -----------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (wrong_date in list_wrong_date_ts) {
        testthat::expect_error(previous_date_ts(date_ts = wrong_date, frequency_ts = 12L, lag = 4L))
        testthat::expect_error(previous_date_ts(date_ts = wrong_date, frequency_ts = 12L, lag = 4L))
    }
})

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(previous_date_ts(
            date_ts = create_random_date_ts(),
            frequency_ts = wrong_frequency, lag = 4L
        ))
    }
})

testthat::test_that("miscellaneous frequency are not allowed", {

    for (wrong_lag in list_wrong_date_ts) {
        date_ts <- create_random_date_ts(frequency_ts = 12L)
        testthat::expect_error(previous_date_ts(
            date_ts = date_ts,
            frequency_ts = 12L, lag = wrong_lag
        ))

        date_ts <- create_random_date_ts(frequency_ts = 4L)
        testthat::expect_error(previous_date_ts(
            date_ts = date_ts,
            frequency_ts = 4L, lag = wrong_lag
        ))
    }
})

# Tests à la main

testthat::expect_identical(
    object = previous_date_ts(2020L, frequency_ts = 12L, lag = 2L),
    expected = c(2019L, 11L)
)
testthat::expect_identical(
    object = previous_date_ts(c(2020L, 5L), frequency_ts = 12L, lag = 5L),
    expected = c(2019L, 12L)
)
testthat::expect_identical(
    object = previous_date_ts(2020L, frequency_ts = 12L, lag = 0L),
    expected = c(2020L, 1L)
)
