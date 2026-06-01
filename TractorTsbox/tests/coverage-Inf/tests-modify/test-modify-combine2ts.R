# Initialisation ---------------------------------------------------------------

set.seed(2022L)

# Tests de résultat ------------------------------------------------------------

typeA <- "complex"
for (typeA in list_type) {
    for (frequenceA in list_frequence) {
        for (startA in list_start) {
            for (lenA in list_len[-1L]) {
                A_content <- create_random_type(type = typeA, len = lenA)
                ts_A <- ts(A_content, start = startA, frequency = frequenceA)
                for (param1 in list_len) {
                    for (param2 in list_len) {
                        test_name <- paste0(
                            "expected result with ",
                            "\ntypeA = '", typeA,
                            "'\nfrequenceA = ", frequenceA,
                            "\nstartA = ", deparse(startA),
                            "\nlenA = ", lenA,
                            "\nparam1 = ", param1,
                            "\nparam2 = ", param2
                        )


                        testthat::test_that(desc = test_name, {
                            # Cas 1
                            if (param1 < lenA & param1 + param2 > 0L) {
                                B1_content <- create_random_type(type = typeA, len = param1 + param2)
                                startB1 <- end(ts_A)
                                if (length(startB1) == 1L) startB1 <- c(startB1, 1L)
                                startB1[2L] <- startB1[2L] - (param1 - 1L)
                                ts_B1 <- ts(B1_content, start = startB1, frequency = frequenceA)
                                # ts_B1 <- ts(
                                #     data = B1_content,
                                #     start = date_ts2timeunits(
                                #         as.integer(end(ts_A)),
                                #         frequency = frequenceA) - (param1 - 1L) / frequenceA,
                                #     frequency = frequenceA
                                # )

                                ts_ResAB1 <- ts(c(A_content[1L:(lenA - param1)], B1_content), start = startA, frequency = frequenceA)
                                if (param2 == 0L) {
                                    ts_ResB1A <- ts_A
                                } else {
                                    ts_ResB1A <- ts(c(A_content, B1_content[(param1 + 1L):(param1 + param2)]), start = startA, frequency = frequenceA)
                                }

                                if (param2 > 0L) {
                                    testthat::expect_warning(
                                        {
                                            resAB1 <- combine2ts(ts_A, ts_B1)
                                        },
                                        regexp = warning_extend
                                    )
                                } else {
                                    resAB1 <- combine2ts(ts_A, ts_B1)
                                }
                                testthat::expect_warning(
                                    {
                                        resB1A <- combine2ts(ts_B1, ts_A)
                                    },
                                    regexp = warning_extend
                                )
                                if (typeA != "Date") {
                                    testthat::expect_type(resAB1, typeA)
                                    testthat::expect_type(resB1A, typeA)
                                }

                                testthat::expect_equal(resAB1, ts_ResAB1)
                                testthat::expect_equal(resB1A, ts_ResB1A)
                            }


                            # Cas 2
                            if (param2 < lenA & param1 + param2 > 0L) {
                                B2_content <- create_random_type(type = typeA, len = param1 + param2)
                                startB2 <- startA
                                if (length(startB2) == 1L) startB2 <- c(startB2, 1L)
                                startB2[2L] <- startB2[2L] - param1
                                ts_B2 <- ts(B2_content, start = startB2, frequency = frequenceA)
                                # ts_B2 <- ts(B2_content,  start = date_ts2timeunits(startA, frequency = frequenceA) - param1 / frequenceA, frequency = frequenceA)

                                ts_ResAB2 <- ts(c(B2_content, A_content[(param2 + 1L):lenA]), start = start(ts_B2), frequency = frequenceA)
                                if (param1 == 0L) {
                                    ts_ResB2A <- ts_A
                                } else {
                                    ts_ResB2A <- ts(c(B2_content[1L:param1], A_content), start = start(ts_B2), frequency = frequenceA)
                                }

                                if (param1 > 0L) {
                                    testthat::expect_warning(
                                        {
                                            resAB2 <- combine2ts(ts_A, ts_B2)
                                        },
                                        regexp = warning_extend
                                    )
                                } else {
                                    resAB2 <- combine2ts(ts_A, ts_B2)
                                }
                                testthat::expect_warning(
                                    {
                                        resB2A <- combine2ts(ts_B2, ts_A)
                                    },
                                    regexp = warning_extend
                                )

                                if (typeA != "Date") {
                                    testthat::expect_type(resAB2, typeA)
                                    testthat::expect_type(resB2A, typeA)
                                }

                                testthat::expect_equal(resAB2, ts_ResAB2)
                                testthat::expect_equal(resB2A, ts_ResB2A)
                            }


                            # Cas 3
                            if (param1 > 0L) {
                                B3_content <- create_random_type(type = typeA, len = param1)
                                startB3 <- startA
                                if (length(startB3) == 1L) startB3 <- c(startB3, 1L)
                                startB3[2L] <- startB3[2L] - (param1 + param2)
                                ts_B3 <- ts(B3_content, start = startB3, frequency = frequenceA)
                                # ts_B3 <- ts(
                                #     data = B3_content,
                                #     start = date_ts2timeunits(
                                #         startA,
                                #         frequency = frequenceA) - (param1 + param2) / frequenceA,
                                #     frequency = frequenceA
                                # )

                                if (typeA == "raw") {
                                    ts_ResAB3 <- ts(c(B3_content, rep(as.raw(0L), param2), A_content), start = start(ts_B3), frequency = frequenceA)
                                    ts_ResB3A <- ts(c(B3_content, rep(as.raw(0L), param2), A_content), start = start(ts_B3), frequency = frequenceA)
                                    if (param2 > 0L) {
                                        testthat::expect_warning(
                                            testthat::expect_warning(
                                                {
                                                    resAB3 <- combine2ts(ts_A, ts_B3)
                                                },
                                                regexp = warning_extend
                                            ),
                                            regexp = "out-of-range values treated as 0 in coercion to raw"
                                        )
                                        testthat::expect_warning(
                                            testthat::expect_warning(
                                                {
                                                    resB3A <- combine2ts(ts_B3, ts_A)
                                                },
                                                regexp = warning_extend
                                            ),
                                            regexp = "out-of-range values treated as 0 in coercion to raw"
                                        )
                                    } else {
                                        testthat::expect_warning(
                                            {
                                                resAB3 <- combine2ts(ts_A, ts_B3)
                                            },
                                            regexp = warning_extend
                                        )
                                        testthat::expect_warning(
                                            {
                                                resB3A <- combine2ts(ts_B3, ts_A)
                                            },
                                            regexp = warning_extend
                                        )
                                    }
                                } else {
                                    ts_ResAB3 <- ts(c(B3_content, rep(NA, param2), A_content), start = start(ts_B3), frequency = frequenceA)
                                    ts_ResB3A <- ts(c(B3_content, rep(NA, param2), A_content), start = start(ts_B3), frequency = frequenceA)
                                    testthat::expect_warning(
                                        {
                                            resAB3 <- combine2ts(ts_A, ts_B3)
                                        },
                                        regexp = warning_extend
                                    )
                                    testthat::expect_warning(
                                        {
                                            resB3A <- combine2ts(ts_B3, ts_A)
                                        },
                                        regexp = warning_extend
                                    )
                                }

                                if (typeA != "Date") {
                                    testthat::expect_type(resAB3, typeA)
                                    testthat::expect_type(resB3A, typeA)
                                }

                                testthat::expect_equal(resAB3, ts_ResAB3)
                                testthat::expect_equal(resB3A, ts_ResB3A)
                            }


                            # Cas 4
                            if (param2 > 0L) {
                                B4_content <- create_random_type(type = typeA, len = param2)
                                startB4 <- end(ts_A)
                                if (length(startB4) == 1L) startB4 <- c(startB4, 1L)
                                startB4[2L] <- startB4[2L] + param1 + 1L
                                ts_B4 <- ts(B4_content, start = startB4, frequency = frequenceA)
                                # ts_B4 <- ts(
                                #     data = B4_content,
                                #     start = date_ts2timeunits(
                                #         as.integer(end(ts_A)),
                                #         frequency = frequenceA) + (param1 + 1L) / frequenceA,
                                #     frequency = frequenceA
                                # )

                                if (typeA == "raw") {
                                    ts_ResAB4 <- ts(c(A_content, rep(as.raw(0L), param1), B4_content), start = startA, frequency = frequenceA)
                                    ts_ResB4A <- ts(c(A_content, rep(as.raw(0L), param1), B4_content), start = startA, frequency = frequenceA)
                                    if (param1 > 0L) {
                                        testthat::expect_warning(
                                            testthat::expect_warning(
                                                {
                                                    resAB4 <- combine2ts(ts_A, ts_B4)
                                                },
                                                regexp = warning_extend
                                            ),
                                            regexp = "out-of-range values treated as 0 in coercion to raw"
                                        )
                                        testthat::expect_warning(
                                            testthat::expect_warning(
                                                {
                                                    resB4A <- combine2ts(ts_B4, ts_A)
                                                },
                                                regexp = warning_extend
                                            ),
                                            regexp = "out-of-range values treated as 0 in coercion to raw"
                                        )
                                    } else {
                                        testthat::expect_warning(
                                            {
                                                resAB4 <- combine2ts(ts_A, ts_B4)
                                            },
                                            regexp = warning_extend
                                        )
                                        testthat::expect_warning(
                                            {
                                                resB4A <- combine2ts(ts_B4, ts_A)
                                            },
                                            regexp = warning_extend
                                        )
                                    }
                                } else {
                                    ts_ResAB4 <- ts(c(A_content, rep(NA, param1), B4_content), start = startA, frequency = frequenceA)
                                    ts_ResB4A <- ts(c(A_content, rep(NA, param1), B4_content), start = startA, frequency = frequenceA)
                                    testthat::expect_warning(
                                        {
                                            resAB4 <- combine2ts(ts_A, ts_B4)
                                        },
                                        regexp = warning_extend
                                    )
                                    testthat::expect_warning(
                                        {
                                            resB4A <- combine2ts(ts_B4, ts_A)
                                        },
                                        regexp = warning_extend
                                    )
                                }

                                if (typeA != "Date") {
                                    testthat::expect_type(resAB4, typeA)
                                    testthat::expect_type(resB4A, typeA)
                                }

                                testthat::expect_equal(resAB4, ts_ResAB4)
                                testthat::expect_equal(resB4A, ts_ResB4A)
                            }


                            # Cas 5
                            B5_content <- create_random_type(type = typeA, len = param1 + param2 + lenA)
                            startB5 <- startA
                            if (length(startB5) == 1L) startB5 <- c(startB5, 1L)
                            startB5[2L] <- startB5[2L] - param1
                            ts_B5 <- ts(B5_content, start = startB5, frequency = frequenceA)
                            # ts_B5 <- ts(B5_content,  start = date_ts2timeunits(startA, frequency = frequenceA) - param1 / frequenceA, frequency = frequenceA)
                            ts_ResAB5 <- ts_B5

                            if (param1 == 0L & param2 == 0L) {
                                ts_ResB5A <- ts_A
                            } else if (param1 == 0L) {
                                ts_ResB5A <- ts(c(A_content, B5_content[(param1 + lenA + 1):(param1 + param2 + lenA)]), start = start(ts_B5), frequency = frequenceA)
                            } else if (param2 == 0L) {
                                ts_ResB5A <- ts(c(B5_content[1L:param1], A_content), start = start(ts_B5), frequency = frequenceA)
                            } else {
                                ts_ResB5A <- ts(
                                    data = c(B5_content[1L:param1],
                                             A_content,
                                             B5_content[(param1 + lenA + 1):(param1 + param2 + lenA)]),
                                    start = start(ts_B5),
                                    frequency = frequenceA
                                )
                            }

                            if (param1 + param2 > 0L) {
                                testthat::expect_warning(
                                    {
                                        resAB5 <- combine2ts(ts_A, ts_B5)
                                    },
                                    regexp = warning_extend
                                )
                            } else {
                                resAB5 <- combine2ts(ts_A, ts_B5)
                            }
                            resB5A <- combine2ts(ts_B5, ts_A)

                            if (typeA != "Date") {
                                testthat::expect_type(resAB5, typeA)
                                testthat::expect_type(resB5A, typeA)
                            }

                            testthat::expect_equal(resAB5, ts_ResAB5)
                            testthat::expect_equal(resB5A, ts_ResB5A)


                            # Cas 6
                            if (param1 + param2 < lenA & param2 > 0L) {
                                B6_content <- create_random_type(type = typeA, len = param2)
                                startB6 <- startA
                                if (length(startB6) == 1L) startB6 <- c(startB6, 1L)
                                startB6[2L] <- startB6[2L] + param1
                                ts_B6 <- ts(B6_content, start = startB6, frequency = frequenceA)
                                # ts_B6 <- ts(B6_content,  start = date_ts2timeunits(startA, frequency = frequenceA) + param1 / frequenceA, frequency = frequenceA)
                                if (param1 == 0L) {
                                    ts_ResAB6 <- ts(c(B6_content, A_content[(param1 + param2 + 1L):lenA]), start = startA, frequency = frequenceA)
                                } else {
                                    ts_ResAB6 <- ts(c(A_content[1L:param1], B6_content, A_content[(param1 + param2 + 1L):lenA]), start = startA, frequency = frequenceA)
                                }
                                ts_ResB6A <- ts_A

                                resAB6 <- combine2ts(ts_A, ts_B6)
                                testthat::expect_warning(
                                    {
                                        resB6A <- combine2ts(ts_B6, ts_A)
                                    },
                                    regexp = warning_extend
                                )

                                if (typeA != "Date") {
                                    testthat::expect_type(resAB6, typeA)
                                    testthat::expect_type(resB6A, typeA)
                                }

                                testthat::expect_equal(resAB6, ts_ResAB6)
                                testthat::expect_equal(resB6A, ts_ResB6A)
                            }
                        })
                    }
                }
            }
        }
    }
}

# Tests sur les erreurs de mts -------------------------------------------------

testthat::test_that("Several dimensions are not allowed", {
    for (typeA in list_type) {
        for (frequenceA in list_frequence) {
            for (startA in list_start) {
                for (lenA in list_len[-1L]) {
                    ts_A <- create_random_ts(type = typeA, start = startA, frequency = frequenceA, len = lenA)
                    B_content <- as.data.frame(lapply(1L:5L, function(i) create_random_type(type = typeA, len = 100L)))
                    startB <- create_random_date_ts()

                    if (typeA == "complex") {
                        mts_B <- do.call(
                            what = cbind,
                            args = lapply(
                                X = B_content,
                                FUN = ts,
                                start = startB,
                                frequency = frequenceA
                            )
                        )
                    } else {
                        mts_B <- ts(B_content, start = startB, frequency = frequenceA)
                    }

                    testthat::expect_error(
                        object = combine2ts(a = ts_A, b = mts_B),
                        regexp = "Variable 'b': Must be of type 'atomic vector'"
                    )
                    testthat::expect_error(
                        object = combine2ts(a = mts_B, b = ts_A),
                        regexp = "Variable 'a': Must be of type 'atomic vector'"
                    )
                }
            }
        }
    }
})

# Tests sur les erreurs d'input ------------------------------------------------

testthat::test_that("miscellaneous input are not allowed", {
    for (typeA in list_type) {
        ts_A <- create_random_ts(type = typeA)

        for (objA in object_bank_R) {
            testthat::expect_error(
                combine2ts(ts_A, objA)
            )
            testthat::expect_error(
                combine2ts(objA, ts_A)
            )
            for (objB in object_bank_R) {
                testthat::expect_error(
                    combine2ts(objA, objB)
                )
            }
        }
    }
})

# Tests sur les erreurs de type d'objets ---------------------------------------

testthat::test_that("different input type are not allowed", {
    for (typeA in list_type[-7L]) {
        objA <- create_random_ts(type = typeA, frequency = 12L)
        for (typeB in list_type[-7L]) {
            objB <- create_random_ts(type = typeB, frequency = 12L)
            if (typeA != typeB) testthat::expect_error(combine2ts(objA, objB), regexp = "Les objets `a` et `b` doivent \u00eatre de m\u00eame type.")
        }
    }
})

# Tests sur les erreurs de temporalité -----------------------------------------

testthat::test_that("arguments have same frequency", {
    for (typeA in list_type) {
        objA <- create_random_ts(type = typeA, frequency = 12L)
        objB <- create_random_ts(type = typeA, frequency = 4L)
        testthat::expect_error(combine2ts(objA, objB), regexp = "Les objets `a` et `b` doivent avoir la m\u00eame fr\u00e9quence.")
        testthat::expect_error(combine2ts(objB, objA), regexp = "Les objets `a` et `b` doivent avoir la m\u00eame fr\u00e9quence.")
    }
})

testthat::test_that("arguments are monthly or quarterly", {
    for (typeA in list_type) {
        for (freq_A in c(weird_frequency)) {
            for (freq_B in c(weird_frequency, list_frequence)) {
                objA <- create_random_ts(type = typeA, frequency = freq_A)
                objB <- create_random_ts(type = typeA, frequency = freq_B)
                testthat::expect_error(combine2ts(objA, objB))
                testthat::expect_error(combine2ts(objB, objA))
            }
        }
    }
})

testthat::test_that("arguments are temporally consistent", {
    for (typeA in list_type) {
        ts_A <- create_random_ts(type = typeA, start = 2015L, frequency = 12L)
        ts_B <- create_random_ts(type = typeA, start = 2004 + 1 / 7, frequency = 12L)
        testthat::expect_error(combine2ts(ts_A, ts_B))
        testthat::expect_error(combine2ts(ts_B, ts_A))

        ts_A <- create_random_ts(type = typeA, start = 2015L, frequency = 4L)
        ts_B <- create_random_ts(type = typeA, start = 2016 + 1 / 12, frequency = 4L)
        testthat::expect_error(combine2ts(ts_A, ts_B))
        testthat::expect_error(combine2ts(ts_B, ts_A))
    }
})
