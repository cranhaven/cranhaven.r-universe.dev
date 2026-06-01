# Initialisation ---------------------------------------------------------------

set.seed(2046L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result with Date", {

    for (n in list_len[-1L]) {
        testthat::expect_silent(substr_year(date = Sys.Date(), n = n))
    }

    for (year in good_years[-1L:-2L]) {
        for (month in good_months) {
            for (day in seq_len(28L)) {
                dateA <- as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")

                for (n in list_len[-1L]) {
                    if (year - n >= 0) {
                        date_theo <- as.Date(paste0(year - n, "-", month, "-", day), format = "%Y-%m-%d")
                        testthat::expect_identical(
                            object = substr_year(date = dateA, n = n),
                            expected = date_theo
                        )
                    }

                }
            }
        }
    }
})


# Test de résultat positif avec warnings ---------------------------------------

## n ---------------------------------------------------------------------------

testthat::test_that("good result with warning n", {

    for (n in list_len[-1L]) {
        n <- as.double(n)
        testthat::expect_warning(
            {res <- substr_year(date = Sys.Date(), n = n)},
            regexp = message_double("n")
        )
    }

    for (year in good_years[-1L:-2L]) {
        for (month in good_months) {
            for (day in seq_len(28L)) {
                dateA <- as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")

                for (n in list_len[-1L]) {
                    if (year - n >= 0L) {

                        n <- as.double(n)

                        date_theo <- as.Date(paste0(year - n, "-", month, "-", day), format = "%Y-%m-%d")
                        testthat::expect_warning(
                            {res <- substr_year(date = dateA, n = n)},
                            regexp = message_double("n")
                        )
                        testthat::expect_identical(res, date_theo)
                    }

                }
            }
        }
    }
})

# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (wrong_date in object_bank_R[-34L]) {
        testthat::expect_error(substr_year(date = wrong_date, n = 12L))
    }
    for (wrong_date in object_bank_R[-34L]) {
        testthat::expect_error(substr_year(date = wrong_date, n = 4L))
    }
})

testthat::test_that("miscelanous n are not accepted", {

    list_wrong_n <- c(list(0., 0L),
                      list_wrong_date_ts,
                      object_bank_R[-10L],
                      rnorm(10L),
                      as.double(-abs(c(list_lag, list_len, create_random_type("integer", len = 10L)))),
                      -abs(c(list_len, list_lag, create_random_type("integer", len = 10L))))


    index_error_warning_n <- c(
        1L, 17L:18L, 35L:36L, 46L, 59L, 62L, 66L:72L, 81L, 88L:97L,
        112L:113L, 130L:131L, 141L, 144L:181L
    )
    index_error_n <- setdiff(seq_along(list_wrong_n), index_error_warning_n)

    # Just errors
    for (wrong_n in list_wrong_n[index_error_n]) {
        testthat::expect_error(substr_year(date = create_random_type(type = "Date", len = 1L), n = wrong_n))
    }

    # Errors + Warnings
    for (wrong_n in list_wrong_n[index_error_warning_n]) {
        testthat::expect_warning(
            testthat::expect_error(
                substr_year(date = create_random_type(type = "Date", len = 1L), n = wrong_n)
            )
        )
    }
})
