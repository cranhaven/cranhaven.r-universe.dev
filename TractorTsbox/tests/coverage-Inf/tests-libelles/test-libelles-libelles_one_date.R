# Initialisation ---------------------------------------------------------------

set.seed(2025L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer date", {
    for (month in good_months) {
        for (year in good_years) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_identical(
                libelles_one_date(date_ts = c(year, month), frequency_ts = 12L),
                paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
            )
        }
    }
})

testthat::test_that("good result for integer date", {
    for (quarter in good_quarters) {
        for (year in good_years) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_identical(
                libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4L),
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
        }
    }
})


# Test de résultats positifs avec warning ---------------------------------

testthat::test_that("warning for integer date out of range", {
    for (month in warning_integer_months) {
        for (year in good_years) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_warning(
                {
                    libel1 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12L, warn = TRUE)
                    testthat::expect_identical(
                        libel1,
                        paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
                    )
                },
                regexp = "Assertion on 'period' failed: Element 1 is not <= 12.0|Assertion on 'period' failed: Element 1 is not >= 1."
            )
            testthat::expect_warning(
                {
                    libel2 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12L)
                    testthat::expect_identical(
                        libel2,
                        paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
                    )
                },
                regexp = "Assertion on 'period' failed: Element 1 is not <= 12.0|Assertion on 'period' failed: Element 1 is not >= 1."
            )
            testthat::expect_no_warning(
                {
                    libel3 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12L, warn = FALSE)
                    testthat::expect_identical(
                        libel3,
                        paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
                    )
                }
            )
        }
    }
})

testthat::test_that("warning for integer date out of range", {
    for (quarter in warning_integer_quarters) {
        for (year in good_years) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_warning(
                {
                    libel1 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4L, warn = TRUE)
                },
                regexp = "Assertion on 'period' failed: Element 1 is not <= 4.0|Assertion on 'period' failed: Element 1 is not >= 1."
            )
            testthat::expect_warning(
                {
                    libel2 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4L)
                },
                regexp = "Assertion on 'period' failed: Element 1 is not <= 4.0|Assertion on 'period' failed: Element 1 is not >= 1."
            )
            testthat::expect_no_warning(
                {
                    libel3 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4L, warn = FALSE)
                }
            )
            testthat::expect_identical(
                libel1,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
            testthat::expect_identical(
                libel2,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
            testthat::expect_identical(
                libel3,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
        }
    }
})

testthat::test_that("warning for double date", {
    for (month in double_months) {
        for (year in good_years) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_warning(
                {
                    libel1 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12L, warn = TRUE)
                },
                regexp = message_double("date_ts")
            )
            testthat::expect_warning(
                {
                    libel2 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12L)
                },
                regexp = message_double("date_ts")
            )
            testthat::expect_no_warning(
                {
                    libel3 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12L, warn = FALSE)
                }
            )
            testthat::expect_identical(
                libel1,
                paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
            )
            testthat::expect_identical(
                libel2,
                paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
            )
            testthat::expect_identical(
                libel3,
                paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
            )
        }
    }

    for (month in good_months) {
        for (year in double_years) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_warning(
                {
                    libel1 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12L, warn = TRUE)
                },
                regexp = message_double("date_ts")
            )
            testthat::expect_warning(
                {
                    libel2 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12L)
                },
                regexp = message_double("date_ts")
            )
            testthat::expect_no_warning(
                {
                    libel3 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12L, warn = FALSE)
                }
            )
            testthat::expect_identical(
                libel1,
                paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
            )
            testthat::expect_identical(
                libel2,
                paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
            )
            testthat::expect_identical(
                libel3,
                paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
            )
        }
    }

    for (quarter in double_quarters) {
        for (year in good_years) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_warning(
                {
                    libel1 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4L, warn = TRUE)
                },
                regexp = message_double("date_ts")
            )
            testthat::expect_warning(
                {
                    libel2 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4L)
                },
                regexp = message_double("date_ts")
            )
            testthat::expect_no_warning(
                {
                    libel3 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4L, warn = FALSE)
                }
            )
            testthat::expect_identical(
                libel1,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
            testthat::expect_identical(
                libel2,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
            testthat::expect_identical(
                libel3,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
        }
    }

    for (quarter in good_quarters) {
        for (year in double_years) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_warning(
                {
                    libel1 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4L, warn = TRUE)
                },
                regexp = message_double("date_ts")
            )
            testthat::expect_warning(
                {
                    libel2 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4L)
                },
                regexp = message_double("date_ts")
            )
            testthat::expect_no_warning(
                {
                    libel3 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4L, warn = FALSE)
                }
            )
            testthat::expect_identical(
                libel1,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
            testthat::expect_identical(
                libel2,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
            testthat::expect_identical(
                libel3,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
        }
    }
})

testthat::test_that("warning for double date and out of range", {
    for (month in warning_double_months) {
        for (year in good_years) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_warning(
                testthat::expect_warning(
                    object = {
                        libel1 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12L, warn = TRUE)
                    },
                    regexp = "Assertion on 'period' failed: Element 1 is not <= 12.0|Assertion on 'period' failed: Element 1 is not >= 1."
                ),
                regexp = message_double("date_ts")
            )
            testthat::expect_warning(
                testthat::expect_warning(
                    object = {
                        libel2 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12L)
                    },
                    regexp = "Assertion on 'period' failed: Element 1 is not <= 12.0|Assertion on 'period' failed: Element 1 is not >= 1."
                ),
                regexp = message_double("date_ts")
            )
            testthat::expect_no_warning(
                object = {
                    libel3 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12L, warn = FALSE)
                }
            )
            testthat::expect_identical(
                libel1,
                paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
            )
            testthat::expect_identical(
                libel2,
                paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
            )
            testthat::expect_identical(
                libel3,
                paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
            )
        }
    }

    for (quarter in warning_double_quarters) {
        for (year in good_years) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_warning(
                testthat::expect_warning(
                    object = {
                        libel1 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4L, warn = TRUE)
                    },
                    regexp = "Assertion on 'period' failed: Element 1 is not <= 4.0|Assertion on 'period' failed: Element 1 is not >= 1."
                ),
                regexp = message_double("date_ts")
            )
            testthat::expect_warning(
                testthat::expect_warning(
                    object = {
                        libel2 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4L)
                    },
                    regexp = "Assertion on 'period' failed: Element 1 is not <= 4.0|Assertion on 'period' failed: Element 1 is not >= 1."
                ),
                regexp = message_double("date_ts")
            )
            testthat::expect_no_warning(
                object = {
                    libel3 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4L, warn = FALSE)
                }
            )
            testthat::expect_identical(
                libel1,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
            testthat::expect_identical(
                libel2,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
            testthat::expect_identical(
                libel3,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
        }
    }
})

testthat::test_that("warning for double monthly frequency", {
    for (month in good_months) {
        for (year in good_years) {
            real_year <- year + (month - 1L) %/% 12L
            testthat::expect_warning(
                {
                    libel1 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12.0, warn = TRUE)
                },
                regexp = message_double("frequency_ts")
            )
            testthat::expect_warning(
                {
                    libel2 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12.0)
                },
                regexp = message_double("frequency_ts")
            )
            testthat::expect_no_warning(
                {
                    libel3 <- libelles_one_date(date_ts = c(year, month), frequency_ts = 12.0, warn = FALSE)
                }
            )
            testthat::expect_identical(
                libel1,
                paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
            )
            testthat::expect_identical(
                libel2,
                paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
            )
            testthat::expect_identical(
                libel3,
                paste(list_months_name[(month - 1L) %% 12L + 1L], real_year)
            )
        }
    }
})

testthat::test_that("warning for double quaterly frequency", {
    for (quarter in good_quarters) {
        for (year in good_years) {
            real_year <- year + (quarter - 1L) %/% 4L
            testthat::expect_warning(
                {
                    libel1 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4.0, warn = TRUE)
                },
                regexp = message_double("frequency_ts")
            )
            testthat::expect_warning(
                {
                    libel2 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4.0)
                },
                regexp = message_double("frequency_ts")
            )
            testthat::expect_no_warning(
                {
                    libel3 <- libelles_one_date(date_ts = c(year, quarter), frequency_ts = 4.0, warn = FALSE)
                }
            )
            testthat::expect_identical(
                libel1,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
            testthat::expect_identical(
                libel2,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
            testthat::expect_identical(
                libel3,
                paste0("Q", (quarter - 1L) %% 4L + 1L, " ", real_year)
            )
        }
    }
})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (wrong_date in list_wrong_date_ts) {
        testthat::expect_error(libelles_one_date(date_ts = wrong_date, frequency_ts = 12L, warn = FALSE))
        testthat::expect_error(libelles_one_date(date_ts = wrong_date, frequency_ts = 4L, warn = FALSE))
    }
})

testthat::test_that("miscellaneous frequency are not allowed", {
    for (wrong_frequency in c(object_bank_R, weird_frequency)) {
        testthat::expect_error(libelles_one_date(date_ts = create_random_date_ts(), frequency_ts = wrong_frequency, warn = FALSE))
    }
})
