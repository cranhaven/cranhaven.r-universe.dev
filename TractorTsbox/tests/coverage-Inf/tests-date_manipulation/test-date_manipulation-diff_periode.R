# Initialisation ---------------------------------------------------------------

set.seed(2033L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result", {
    for (year in good_years) {
        for (len in list_lag) {
            date_a <- year
            date_b <- year + len

            res1 <- diff_periode(date_a, date_b, frequency_ts = 12L)
            res2 <- diff_periode(date_b, date_a, frequency_ts = 12L)

            testthat::expect_type(res1, "integer")
            testthat::expect_type(res2, "integer")
            testthat::expect_identical(res1, abs(len) * 12L + 1L)
            testthat::expect_identical(res2, abs(len) * 12L + 1L)

            res1 <- diff_periode(date_a, date_b, frequency_ts = 4L)
            res2 <- diff_periode(date_b, date_a, frequency_ts = 4L)

            testthat::expect_type(res1, "integer")
            testthat::expect_type(res2, "integer")
            testthat::expect_identical(res1, abs(len) * 4L + 1L)
            testthat::expect_identical(res2, abs(len) * 4L + 1L)

            for (month in good_months) {
                date_a <- c(year, month)
                date_b <- c(
                    year + (month + len - 1L) %/% 12L,
                    (month + len - 1L) %% 12L + 1L
                )

                res1 <- diff_periode(date_a, date_b, frequency_ts = 12L)
                res2 <- diff_periode(date_b, date_a, frequency_ts = 12L)

                testthat::expect_type(res1, "integer")
                testthat::expect_type(res2, "integer")
                testthat::expect_identical(res1, abs(len) + 1L)
                testthat::expect_identical(res2, abs(len) + 1L)
            }

            for (quarter in good_quarters) {
                date_a <- c(year, quarter)
                date_b <- c(
                    year + (quarter + len - 1L) %/% 4L,
                    (quarter + len - 1L) %% 4L + 1L
                )

                res1 <- diff_periode(date_a, date_b, frequency_ts = 4L)
                res2 <- diff_periode(date_b, date_a, frequency_ts = 4L)

                testthat::expect_type(res1, "integer")
                testthat::expect_type(res2, "integer")
                testthat::expect_identical(res1, abs(len) + 1L)
                testthat::expect_identical(res2, abs(len) + 1L)
            }
        }
    }
})


# Tests de résultats positif avec warning --------------------------------------

testthat::test_that("warning with double years", {
    for (year in double_years) {
        for (len in list_lag) {
            date_a <- year
            date_b <- year + len

            w <- testthat::capture_warnings({
                res1 <- diff_periode(date_a, date_b, frequency_ts = 12L)
            })
            testthat::expect_match(
                object = w,
                regexp = message_double("a"),
                all = FALSE
            )
            testthat::expect_match(
                object = w,
                regexp = message_double("a"),
                all = FALSE
            )

            w <- testthat::capture_warnings({
                res2 <- diff_periode(date_b, date_a, frequency_ts = 12L)
            })
            testthat::expect_match(
                object = w,
                regexp = message_double("a"),
                all = FALSE
            )
            testthat::expect_match(
                object = w,
                regexp = message_double("a"),
                all = FALSE
            )

            testthat::expect_type(res1, "integer")
            testthat::expect_type(res2, "integer")
            testthat::expect_identical(res1, abs(len) * 12L + 1L)
            testthat::expect_identical(res2, abs(len) * 12L + 1L)

            w <- testthat::capture_warnings({
                res1 <- diff_periode(date_a, date_b, frequency_ts = 4L)
            })
            testthat::expect_match(
                object = w,
                regexp = message_double("a"),
                all = FALSE
            )
            testthat::expect_match(
                object = w,
                regexp = "Assertion on 'b' failed: Must be of type 'integer', not 'double'.",
                all = FALSE
            )

            w <- testthat::capture_warnings({
                res2 <- diff_periode(date_b, date_a, frequency_ts = 4L)
            })
            testthat::expect_match(
                object = w,
                regexp = message_double("a"),
                all = FALSE
            )
            testthat::expect_match(
                object = w,
                regexp = "Assertion on 'b' failed: Must be of type 'integer', not 'double'.",
                all = FALSE
            )

            testthat::expect_type(res1, "integer")
            testthat::expect_type(res2, "integer")
            testthat::expect_identical(res1, abs(len) * 4L + 1L)
            testthat::expect_identical(res2, abs(len) * 4L + 1L)

            for (month in c(warning_double_months, warning_integer_months, double_months)) {
                date_a <- c(year, month)
                date_b <- c(year, month + len)

                w <- testthat::capture_warnings({
                    res1 <- diff_periode(date_a, date_b, frequency_ts = 12L)
                })
                testthat::expect_match(
                    object = w,
                    regexp = message_double("a"),
                    all = FALSE
                )
                testthat::expect_match(
                    object = w,
                    regexp = message_double("a"),
                    all = FALSE
                )

                w <- testthat::capture_warnings({
                    res2 <- diff_periode(date_b, date_a, frequency_ts = 12L)
                })
                testthat::expect_match(
                    object = w,
                    regexp = message_double("a"),
                    all = FALSE
                )
                testthat::expect_match(
                    object = w,
                    regexp = message_double("a"),
                    all = FALSE
                )

                testthat::expect_type(res1, "integer")
                testthat::expect_type(res2, "integer")
                testthat::expect_identical(res1, abs(len) + 1L)
                testthat::expect_identical(res2, abs(len) + 1L)
            }

            for (quarter in c(warning_double_quarters, warning_integer_quarters, double_quarters)) {
                date_a <- c(year, quarter)
                date_b <- c(year, quarter + len)

                w <- testthat::capture_warnings({
                    res1 <- diff_periode(date_a, date_b, frequency_ts = 4L)
                })
                testthat::expect_match(
                    object = w,
                    regexp = message_double("a"),
                    all = FALSE
                )
                testthat::expect_match(
                    object = w,
                    regexp = "Assertion on 'b' failed: Must be of type 'integer', not 'double'.",
                    all = FALSE
                )

                w <- testthat::capture_warnings({
                    res2 <- diff_periode(date_b, date_a, frequency_ts = 4L)
                })
                testthat::expect_match(
                    object = w,
                    regexp = message_double("a"),
                    all = FALSE
                )
                testthat::expect_match(
                    object = w,
                    regexp = "Assertion on 'b' failed: Must be of type 'integer', not 'double'.",
                    all = FALSE
                )

                testthat::expect_type(res1, "integer")
                testthat::expect_type(res2, "integer")
                testthat::expect_identical(res1, abs(len) + 1L)
                testthat::expect_identical(res2, abs(len) + 1L)
            }
        }
    }
})

testthat::test_that("warning frequency", {
    for (year in good_years) {
        for (len in list_lag) {
            date_a <- year
            date_b <- year + len

            testthat::expect_warning(
                object = {
                    res1 <- diff_periode(date_a, date_b, frequency_ts = 12.0)
                },
                regexp = message_double("frequency_ts")
            )
            testthat::expect_warning(
                object = {
                    res2 <- diff_periode(date_b, date_a, frequency_ts = 12.0)
                },
                regexp = message_double("frequency_ts")
            )

            testthat::expect_type(res1, "integer")
            testthat::expect_type(res2, "integer")
            testthat::expect_identical(res1, abs(len) * 12L + 1L)
            testthat::expect_identical(res2, abs(len) * 12L + 1L)

            testthat::expect_warning(
                object = {
                    res1 <- diff_periode(date_a, date_b, frequency_ts = 4.0)
                },
                regexp = message_double("frequency_ts")
            )
            testthat::expect_warning(
                object = {
                    res2 <- diff_periode(date_b, date_a, frequency_ts = 4.0)
                },
                regexp = message_double("frequency_ts")
            )

            testthat::expect_type(res1, "integer")
            testthat::expect_type(res2, "integer")
            testthat::expect_identical(res1, abs(len) * 4L + 1L)
            testthat::expect_identical(res2, abs(len) * 4L + 1L)

            for (month in good_months) {
                date_a <- c(year, month)
                date_b <- c(
                    year + (month + len - 1L) %/% 12L,
                    (month + len - 1L) %% 12L + 1L
                )

                testthat::expect_warning(
                    object = {
                        res1 <- diff_periode(date_a, date_b, frequency_ts = 12.0)
                    },
                    regexp = message_double("frequency_ts")
                )
                testthat::expect_warning(
                    object = {
                        res2 <- diff_periode(date_b, date_a, frequency_ts = 12.0)
                    },
                    regexp = message_double("frequency_ts")
                )

                testthat::expect_type(res1, "integer")
                testthat::expect_type(res2, "integer")
                testthat::expect_identical(res1, abs(len) + 1L)
                testthat::expect_identical(res2, abs(len) + 1L)
            }

            for (quarter in good_quarters) {
                date_a <- c(year, quarter)
                date_b <- c(
                    year + (quarter + len - 1L) %/% 4L,
                    (quarter + len - 1L) %% 4L + 1L
                )

                testthat::expect_warning(
                    object = {
                        res1 <- diff_periode(date_a, date_b, frequency_ts = 4.0)
                    },
                    regexp = message_double("frequency_ts")
                )
                testthat::expect_warning(
                    object = {
                        res2 <- diff_periode(date_b, date_a, frequency_ts = 4.0)
                    },
                    regexp = message_double("frequency_ts")
                )

                testthat::expect_type(res1, "integer")
                testthat::expect_type(res2, "integer")
                testthat::expect_identical(res1, abs(len) + 1L)
                testthat::expect_identical(res2, abs(len) + 1L)
            }
        }
    }
})


# Tests de résultats négatifs (erreur) -----------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (wrong_date in list_wrong_date_ts) {
        date_a <- create_random_date_ts() |> normalize_date_ts(frequency_ts = 12L, test = FALSE)
        testthat::expect_error(diff_periode(a = wrong_date, b = date_a, frequency_ts = 12L))
        testthat::expect_error(diff_periode(b = wrong_date, a = date_a, frequency_ts = 12L))

        date_a <- create_random_date_ts() |> normalize_date_ts(frequency_ts = 4L, test = FALSE)
        date_b <- create_random_date_ts() |> normalize_date_ts(frequency_ts = 4L, test = FALSE)
        testthat::expect_error(diff_periode(a = wrong_date, b = date_a, frequency_ts = 4L))
        testthat::expect_error(diff_periode(b = wrong_date, a = date_a, frequency_ts = 4L))
    }
})

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        date_a <- create_random_date_ts() |> normalize_date_ts(frequency_ts = 12L, test = FALSE)
        date_b <- create_random_date_ts() |> normalize_date_ts(frequency_ts = 12L, test = FALSE)

        testthat::expect_error(diff_periode(a = date_a, b = date_b, frequency_ts = wrong_frequency))
    }
})
