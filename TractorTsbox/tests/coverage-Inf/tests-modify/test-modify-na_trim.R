# Initialisation ---------------------------------------------------------------

set.seed(2041L)


# Tests de résultat avec start vecteur d'entiers -------------------------------

for (typeA in list_type[-6L]) {
    for (lenA in list_len[-1L]) {
        A_content <- create_random_type(type = typeA, len = lenA)
        for (frequenceA in list_frequence) {
            for (startA in list_start) {
                ts_without_na <- ts(A_content, start = startA, frequency = frequenceA)
                for (param1 in list_len) {

                    test_name <- paste0(
                        "expected result with ",
                        "\ntypeA = '", typeA,
                        "'\nfrequenceA = ", frequenceA,
                        "\nstartA = ", deparse(startA),
                        "\nlenA = ", lenA,
                        "\nparam1 = ", param1
                    )

                    testthat::test_that(desc = test_name, {

                        # Cas 1
                        ts_with_na <- ts(c(A_content, create_NA_type(type = typeA, len = param1)),
                                         start = startA, frequency = frequenceA)
                        res <- testthat::expect_silent(na_trim(ts_with_na))
                        testthat::expect_equal(
                            object = res,
                            expected = ts_without_na
                        )

                        # Cas 2
                        ts_with_na <- ts(c(create_NA_type(type = typeA, len = param1), A_content),
                                         end = end(ts_without_na), frequency = frequenceA)
                        res <- testthat::expect_silent(na_trim(ts_with_na))
                        testthat::expect_equal(
                            object = res,
                            expected = ts_without_na
                        )

                        # Cas 3
                        for (param2 in list_len[2]) {
                            ts_with_na <- ts(c(create_NA_type(type = typeA, len = param1),
                                               A_content,
                                               create_NA_type(type = typeA, len = param2)),
                                             start = startA, frequency = frequenceA)

                            if (length(startA) == 1L) startA <- c(startA, 1L)
                            startA[2L] <- startA[2L] + param1
                            ts_without_na <- ts(A_content, start = c(startA, param1),
                                                frequency = frequenceA)

                            res <- testthat::expect_silent(na_trim(ts_with_na))
                            testthat::expect_equal(
                                object = res,
                                expected = ts_without_na
                            )
                        }
                    })
                }
            }
        }
    }
}


# Tests sur les erreurs de mts --------------------------------------------

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
                        na_trim(series = mts_B),
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
    for (obj in object_bank_R) {
        testthat::expect_error(na_trim(series = obj))
    }
})


# Tests sur les erreurs de temporalité --------------------------------------------

testthat::test_that("series and date are temporally consistent", {
    for (typeA in list_type) {
        testthat::expect_error(na_trim(
            series = create_random_ts(type = typeA, start = 2010 + 1 / 7, frequency = 12L)
        ))
    }

    for (typeA in list_type) {
        testthat::expect_error(na_trim(
            series = create_random_ts(type = typeA, start = 2022 + 1 / 5, frequency = 4L)
        ))
    }
})
