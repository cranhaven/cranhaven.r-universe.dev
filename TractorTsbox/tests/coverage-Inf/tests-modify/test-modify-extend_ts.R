# Initialisation ---------------------------------------------------------------

set.seed(2044L)


# Tests de résultat avec start vecteur d'entiers -------------------------------

for (typeA in list_type) {
    for (frequenceA in list_frequence) {
        for (startA in list_start) {
            for (lenA in list_len[-1L]) {
                A_content <- create_random_type(type = typeA, len = lenA)
                for (lenB in list_len[-1L]) {
                    B_content <- create_random_type(type = typeA, len = lenB)

                    test_name <- paste0(
                        "expected result with ",
                        "\ntypeA = ", deparse(typeA),
                        "\nfrequenceA = ", deparse(frequenceA),
                        "\nstartA = ", deparse(startA),
                        "\nlenA = ", deparse(lenA),
                        "\nlenB = ", deparse(lenB)
                    )

                    testthat::test_that(test_name, {

                        # Cas 1 : simple
                        ts_A <- ts(A_content, start = startA, frequency = frequenceA)
                        res_theo <- ts(c(A_content, B_content), start = startA, frequency = frequenceA)
                        testthat::expect_warning(
                            {
                                res <- extend_ts(
                                    series = ts_A,
                                    replacement = B_content,
                                    date_ts = NULL,
                                    replace_na = FALSE
                                )
                            },
                            regexp = warning_extend
                        )
                        testthat::expect_equal(expected = res_theo, object = res)

                        # Cas 1.5 : simple (by default)
                        ts_A <- ts(A_content, start = startA, frequency = frequenceA)
                        res_theo <- ts(c(A_content, B_content), start = startA, frequency = frequenceA)
                        testthat::expect_warning(
                            {
                                res <- extend_ts(
                                    series = ts_A,
                                    replacement = B_content
                                )
                            },
                            regexp = warning_extend
                        )
                        testthat::expect_equal(expected = res_theo, object = res)

                        # Cas 2 : with with NA
                        for (param1 in list_len) {
                            ts_A <- ts(
                                c(A_content, create_NA_type(type = typeA, len = param1)),
                                start = startA, frequency = frequenceA
                            )
                            res_theo <- ts(
                                c(A_content, create_NA_type(type = typeA, len = param1), B_content),
                                start = startA, frequency = frequenceA
                            )
                            testthat::expect_warning(
                                {
                                    res <- extend_ts(
                                        series = ts_A,
                                        replacement = B_content,
                                        date_ts = NULL,
                                        replace_na = FALSE
                                    )
                                },
                                regexp = warning_extend
                            )
                            testthat::expect_equal(expected = res_theo, object = res)
                        }

                        # Cas 2.5 : with with NA (by default)
                        for (param1 in list_len) {
                            ts_A <- ts(
                                c(A_content, create_NA_type(type = typeA, len = param1)),
                                start = startA, frequency = frequenceA
                            )
                            res_theo <- ts(
                                c(A_content, create_NA_type(type = typeA, len = param1), B_content),
                                start = startA, frequency = frequenceA
                            )
                            testthat::expect_warning(
                                {
                                    res <- extend_ts(
                                        series = ts_A,
                                        replacement = B_content,
                                        replace_na = FALSE
                                    )
                                },
                                regexp = warning_extend
                            )
                            testthat::expect_equal(expected = res_theo, object = res)
                        }

                        # Cas 3 : with without NA
                        for (param1 in list_len) {
                            ts_A <- ts(
                                c(A_content, create_NA_type(type = typeA, len = param1)),
                                start = startA, frequency = frequenceA
                            )
                            res_theo <- ts(
                                c(A_content, B_content),
                                start = startA, frequency = frequenceA
                            )

                            testthat::expect_warning(
                                {
                                    res <- extend_ts(
                                        series = ts_A,
                                        replacement = B_content,
                                        date_ts = NULL,
                                        replace_na = TRUE
                                    )
                                },
                                regexp = warning_extend
                            )

                            testthat::expect_equal(expected = res_theo, object = res)
                        }

                        # Cas 3.5 : with without NA (by default)
                        for (param1 in list_len) {
                            ts_A <- ts(
                                c(A_content, create_NA_type(type = typeA, len = param1)),
                                start = startA, frequency = frequenceA
                            )
                            res_theo <- ts(
                                c(A_content, B_content),
                                start = startA, frequency = frequenceA
                            )

                            testthat::expect_warning(
                                {
                                    res <- extend_ts(
                                        series = ts_A,
                                        replacement = B_content
                                    )
                                },
                                regexp = warning_extend
                            )

                            testthat::expect_equal(expected = res_theo, object = res)
                        }

                        # Cas 4 : date_ts
                        ts_A <- ts(A_content, start = startA, frequency = frequenceA)

                        for (param1 in list_len[-1L]) {
                            date_end_replacement <- as.integer(end(ts_A))
                            date_end_replacement[2L] <- date_end_replacement[2L] + lenB * param1

                            date_end_replacement[1L] <- date_end_replacement[1L] + (date_end_replacement[2L] - 1L) %/% frequenceA
                            date_end_replacement[2L] <- (date_end_replacement[2L] - 1L) %% frequenceA + 1L

                            res_theo <- ts(c(A_content, rep(B_content, param1)), start = startA, frequency = frequenceA)

                            testthat::expect_warning(
                                {
                                    res <- extend_ts(
                                        series = ts_A,
                                        replacement = B_content,
                                        date_ts = date_end_replacement,
                                        replace_na = FALSE
                                    )
                                },
                                regexp = warning_extend
                            )
                            testthat::expect_equal(expected = res_theo, object = res)
                        }

                        # Cas 5 : date_ts with with NA
                        for (param1 in list_len) {
                            ts_A <- ts(
                                c(A_content, create_NA_type(type = typeA, len = param1)),
                                start = startA, frequency = frequenceA
                            )

                            for (param2 in list_len[-1L]) {
                                date_end_replacement <- as.integer(end(ts_A))
                                date_end_replacement[2L] <- date_end_replacement[2L] + lenB * param2

                                date_end_replacement[1L] <- date_end_replacement[1L] + (date_end_replacement[2L] - 1L) %/% frequenceA
                                date_end_replacement[2L] <- (date_end_replacement[2L] - 1L) %% frequenceA + 1L

                                res_theo <- ts(
                                    c(A_content, create_NA_type(type = typeA, len = param1), rep(B_content, param2)),
                                    start = startA, frequency = frequenceA
                                )
                                testthat::expect_warning(
                                    {
                                        res <- extend_ts(
                                            series = ts_A,
                                            replacement = B_content,
                                            date_ts = date_end_replacement,
                                            replace_na = FALSE
                                        )
                                    },
                                    regexp = warning_extend
                                )
                                testthat::expect_equal(expected = res_theo, object = res)
                            }
                        }

                        # Cas 6 : date_ts with without NA
                        for (param1 in list_len) {
                            ts_A <- ts(
                                c(A_content, create_NA_type(type = typeA, len = param1)),
                                start = startA, frequency = frequenceA
                            )

                            for (param2 in list_len[-1L]) {
                                date_end_replacement <- as.integer(end(ts_A))
                                date_end_replacement[2L] <- date_end_replacement[2L] + lenB * param2 - param1

                                date_end_replacement[1L] <- date_end_replacement[1L] + (date_end_replacement[2L] - 1L) %/% frequenceA
                                date_end_replacement[2L] <- (date_end_replacement[2L] - 1L) %% frequenceA + 1L

                                res_theo <- ts(
                                    c(A_content, rep(B_content, param2)),
                                    start = startA, frequency = frequenceA
                                )
                                testthat::expect_warning(
                                    {
                                        res <- extend_ts(
                                            series = ts_A,
                                            replacement = B_content,
                                            date_ts = date_end_replacement,
                                            replace_na = TRUE
                                        )
                                    },
                                    regexp = warning_extend
                                )
                                testthat::expect_equal(expected = res_theo, object = res)
                            }
                        }

                        # Cas 6.5 : date_ts with without NA (by default)
                        for (param1 in list_len) {
                            ts_A <- ts(
                                c(A_content, create_NA_type(type = typeA, len = param1)),
                                start = startA, frequency = frequenceA
                            )

                            for (param2 in list_len[-1L]) {
                                date_end_replacement <- as.integer(end(ts_A))
                                date_end_replacement[2L] <- date_end_replacement[2L] + lenB * param2 - param1

                                date_end_replacement[1L] <- date_end_replacement[1L] + (date_end_replacement[2L] - 1L) %/% frequenceA
                                date_end_replacement[2L] <- (date_end_replacement[2L] - 1L) %% frequenceA + 1L

                                res_theo <- ts(
                                    c(A_content, rep(B_content, param2)),
                                    start = startA, frequency = frequenceA
                                )
                                testthat::expect_warning(
                                    {
                                        res <- extend_ts(
                                            series = ts_A,
                                            replacement = B_content,
                                            date_ts = date_end_replacement
                                        )
                                    },
                                    regexp = warning_extend
                                )
                                testthat::expect_equal(expected = res_theo, object = res)
                            }
                        }

                        # stop("A compléter avec des appels de extend_ts avec des arguments par défault (non précisés)")


                    })

                }

            }
        }
    }
}


# # Tests sur les erreurs de mts --------------------------------------------

testthat::test_that("Several dimensions are not allowed", {
    for (typeA in list_type) {
        for (frequenceA in list_frequence) {
            for (startA in list_start) {
                for (lenA in list_len[-1L]) {
                    B_content <- as.data.frame(lapply(
                        X = 1L:5L,
                        FUN = function(i) create_random_type(type = typeA, len = lenA)
                    ))

                    if (typeA == "complex") {
                        mts_B <- do.call(
                            what = cbind,
                            args = lapply(
                                X = B_content,
                                FUN = ts,
                                start = startA,
                                frequency = frequenceA
                            )
                        )
                    } else {
                        mts_B <- ts(B_content, start = startA, frequency = frequenceA)
                    }

                    testthat::expect_error(
                        extend_ts(
                            series = mts_B,
                            date_ts = create_random_date_ts(),
                            replacement = create_random_type(type = typeA)
                        ),
                        regexp = "Variable 'series': Must be of type 'atomic vector'"
                    )
                }
            }
        }
    }
})

# Tests sur les erreurs d'input ------------------------------------------------

## Test sur le ts --------------------------------------------------------------

testthat::test_that("miscellaneous series are not allowed", {
    for (typeA in list_type) {
        for (obj in object_bank_R) {
            testthat::expect_error(extend_ts(
                series = obj,
                date_ts = create_random_date_ts(),
                replacement = create_random_type(type = typeA)
            ))
        }
    }
})

## Test sur la date ------------------------------------------------------------

testthat::test_that("miscellaneous date are not allowed", {
    for (typeA in list_type) {
        for (wrong_date in list_wrong_date_ts[-46]) {
            testthat::expect_error(extend_ts(
                series = create_random_ts(type = typeA),
                date_ts = wrong_date,
                replacement = create_random_type(type = typeA)
            ))
        }
    }
})

## Test sur le vecteur value ---------------------------------------------------

testthat::test_that("miscellaneous value input are not allowed", {
    list_wrong_value <- c(fuzzr::test_df()[-4L], NULL, character(0L), numeric(0L), logical(0L), integer(0L), complex(0L))
    for (typeA in list_type) {
        for (value in list_wrong_value) {
            testthat::expect_error(extend_ts(
                series = create_random_ts(type = typeA),
                date_ts = create_random_date_ts(),
                replacement = value
            ))
            testthat::expect_error(extend_ts(
                series = create_random_ts(type = typeA),
                date_ts = NULL,
                replacement = value
            ))
        }
    }
})

testthat::test_that("value should have same type as series", {
    for (typeA in list_type[-7L]) {
        for (typeB in list_type[-7L]) {
            if (typeA != typeB) {
                testthat::expect_error(
                    extend_ts(
                        series = create_random_ts(type = typeA),
                        date_ts = NULL,
                        replacement = create_random_type(typeB)
                    ),
                    regexp = "Les objets `series` et `replacement` doivent \u00eatre de m\u00eame type."
                )
            }
        }
    }
})

# Tests sur les erreurs de temporalité --------------------------------------------

testthat::test_that("series and date are temporally consistent", {
    for (wrong_timeunits in c(object_bank_R[-c(10L, 16L)], list_wrong_timeunits)) {
        for (typeA in list_type) {
            # Monthly
            testthat::expect_error(extend_ts(
                series = create_random_ts(type = typeA, start = wrong_timeunits, frequency = 12L),
                date_ts = create_random_date_ts(),
                replacement = create_random_type(type = typeA)
            ))
            testthat::expect_error(extend_ts(
                series = create_random_ts(type = typeA, start = wrong_timeunits, frequency = 12L),
                date_ts = NULL,
                replacement = create_random_type(type = typeA)
            ))
            testthat::expect_error(extend_ts(
                series = create_random_ts(type = typeA, start = wrong_timeunits, frequency = 12L),
                replacement = create_random_type(type = typeA)
            ))

            # Quaterly
            testthat::expect_error(extend_ts(
                series = create_random_ts(type = typeA, start = wrong_timeunits, frequency = 4L),
                date_ts = create_random_date_ts(),
                replacement = create_random_type(type = typeA)
            ))
            testthat::expect_error(extend_ts(
                series = create_random_ts(type = typeA, start = wrong_timeunits, frequency = 4L),
                date_ts = NULL,
                replacement = create_random_type(type = typeA)
            ))
            testthat::expect_error(extend_ts(
                series = create_random_ts(type = typeA, start = wrong_timeunits, frequency = 4L),
                replacement = create_random_type(type = typeA)
            ))
        }
    }
})

testthat::test_that("date_ts must be after end_ts", {
    for (typeA in list_type) {
        # Monthly
        testthat::expect_error(extend_ts(
            series = create_random_ts(type = typeA, start = 2010L, frequency = 12L, len = 10L),
            date_ts = 2010L,
            replacement = create_random_type(type = typeA)
        ))

        # Quaterly
        testthat::expect_error(extend_ts(
            series = create_random_ts(type = typeA, start = 2022L, frequency = 4L, len = 10L),
            date_ts = 2023L,
            replacement = create_random_type(type = typeA)
        ))
    }
})
