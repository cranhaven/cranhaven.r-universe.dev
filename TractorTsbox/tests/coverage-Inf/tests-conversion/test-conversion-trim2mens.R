# Initialisation ---------------------------------------------------------------

set.seed(2029L)

# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer date", {
    for (good_year in good_years) {
        for (quarter in good_quarters) {
            real_quarter <- (quarter - 1L) %% 4L + 1L
            year_real <- good_year + (quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))
            res <- trim2mens(c(good_year, quarter))


            testthat::expect_identical(res, date_expected)
            testthat::expect_type(res, "integer")
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("detection of wrong dates", {
    for (wrong_date in list_wrong_date_ts) testthat::expect_error(trim2mens(wrong_date))
})


# Tests positifs avec warning --------------------------------------------------

testthat::test_that("warning for integer date", {
    for (good_year in good_years) {
        for (warning_quarter in warning_integer_quarters) {
            testthat::expect_warning(
                {
                    resMonthly <- trim2mens(c(good_year, warning_quarter))
                },
                regexp = "Assertion on 'period' failed: Element 1 is not <= 4.0|Assertion on 'period' failed: Element 1 is not >= 1."
            )

            real_quarter <- (warning_quarter - 1L) %% 4L + 1L
            year_real <- good_year + (warning_quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))

            testthat::expect_identical(resMonthly, date_expected)
            testthat::expect_type(resMonthly, "integer")
        }
    }
})

testthat::test_that("warning for double date", {
    for (warning_year in double_years) {
        for (good_quarter in good_quarters) {
            testthat::expect_warning(
                {
                    resMonthly <- trim2mens(c(warning_year, good_quarter))
                },
                regexp = message_double("date_ts")
            )

            real_quarter <- (good_quarter - 1L) %% 4L + 1L
            year_real <- warning_year + (good_quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))

            testthat::expect_identical(resMonthly, date_expected)
            testthat::expect_type(resMonthly, "integer")
        }
    }

    for (good_year in good_years) {
        for (warning_quarter in double_quarters) {
            testthat::expect_warning(
                {
                    resMonthly <- trim2mens(c(good_year, warning_quarter))
                },
                regexp = message_double("date_ts")
            )

            real_quarter <- (warning_quarter - 1L) %% 4L + 1L
            year_real <- good_year + (warning_quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))

            testthat::expect_identical(resMonthly, date_expected)
            testthat::expect_type(resMonthly, "integer")
        }
    }

    for (warning_year in double_years) {
        for (warning_quarter in double_quarters) {
            testthat::expect_warning(
                {
                    resMonthly <- trim2mens(c(good_year, warning_quarter))
                },
                regexp = message_double("date_ts")
            )

            real_quarter <- (warning_quarter - 1L) %% 4L + 1L
            year_real <- good_year + (warning_quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))

            testthat::expect_identical(resMonthly, date_expected)
            testthat::expect_type(resMonthly, "integer")
        }
    }
})

testthat::test_that("several warning", {
    for (warning_year in double_years) {
        for (warning_quarter in warning_integer_quarters) {
            w <- testthat::capture_warnings({
                resMonthly <- trim2mens(c(warning_year, warning_quarter))
            })
            testthat::expect_match(object = w, regexp = message_double("date_ts"), all = FALSE)
            testthat::expect_match(
                object = w,
                regexp = paste(
                    "Assertion on 'period' failed: Element 1 is not <= 4.0",
                    "Assertion on 'period' failed: Element 1 is not >= 1.", sep = "|"
                ),
                all = FALSE
            )

            real_quarter <- (warning_quarter - 1L) %% 4L + 1L
            year_real <- warning_year + (warning_quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))

            testthat::expect_identical(resMonthly, date_expected)
            testthat::expect_type(resMonthly, "integer")
        }
    }

    for (warning_year in double_years) {
        for (warning_quarter in warning_double_quarters) {
            w <- testthat::capture_warnings({
                resMonthly <- trim2mens(c(warning_year, warning_quarter))
            })
            testthat::expect_match(object = w, regexp = message_double("date_ts"), all = FALSE)
            testthat::expect_match(
                object = w,
                regexp = paste(
                    "Assertion on 'period' failed: Element 1 is not <= 4.0",
                    "Assertion on 'period' failed: Element 1 is not >= 1.", sep = "|"
                ),
                all = FALSE
            )

            real_quarter <- (warning_quarter - 1L) %% 4L + 1L
            year_real <- warning_year + (warning_quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))

            testthat::expect_identical(resMonthly, date_expected)
            testthat::expect_type(resMonthly, "integer")
        }
    }

    for (good_year in good_years) {
        for (warning_quarter in warning_double_quarters) {
            w <- testthat::capture_warnings({
                resMonthly <- trim2mens(c(good_year, warning_quarter))
            })
            testthat::expect_match(object = w, regexp = message_double("date_ts"), all = FALSE)
            testthat::expect_match(
                object = w,
                regexp = paste(
                    "Assertion on 'period' failed: Element 1 is not <= 4.0",
                    "Assertion on 'period' failed: Element 1 is not >= 1.", sep = "|"
                ),
                all = FALSE
            )

            real_quarter <- (warning_quarter - 1L) %% 4L + 1L
            year_real <- good_year + (warning_quarter - 1L) %/% 4L
            date_expected <- as.integer(c(year_real, conversion_quarter_month[real_quarter, "month"]))

            testthat::expect_identical(resMonthly, date_expected)
            testthat::expect_type(resMonthly, "integer")
        }
    }
})
