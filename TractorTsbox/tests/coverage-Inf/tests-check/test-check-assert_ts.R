# Initialisation ---------------------------------------------------------------

set.seed(2032L)


# Tests de résultats positifs = TRUE -------------------------------------------

testthat::test_that("No warning nor error with expected with good TS", {
    for (typeA in list_type) {
        for (frequenceA in list_frequence) {
            for (startA in list_start) {
                for (lenA in list_len[-1L]) {
                    ts_A <- create_random_ts(
                        type = typeA, len = lenA, frequency = frequenceA,
                        start = startA
                    )
                    testthat::expect_silent(assert_ts(ts_A))
                }
            }
        }
    }
})


# Tests de résultats négatif = FALSE -------------------------------------------

### Mauvais objet R ------------------------------------------------------------

testthat::test_that("Result FALSE expected with wrong object R", {
    for (wrong_ts in object_bank_R) {
        testthat::expect_error(assert_ts(wrong_ts))
    }
})

### MTS ------------------------------------------------------------------------

testthat::test_that("Result FALSE expected with mts", {
    for (typeA in list_type) {
        for (lenA in list_len[-c(1L, 9L)]) {
            for (frequenceA in list_frequence) {
                for (len2 in list_len[-c(1L, 2L, 9L)]) {
                    A_content <- as.data.frame(lapply(seq_len(len2), function(i) create_random_type(type = typeA, len = lenA)))
                    startA <- create_random_date_ts()
                    if (typeA == "complex") {
                        mts_A <- do.call(
                            what = cbind,
                            args = lapply(
                                X = A_content,
                                FUN = ts,
                                start = startA,
                                frequency = frequenceA
                            )
                        )
                    } else {
                        mts_A <- ts(A_content, start = startA, frequency = frequenceA)
                    }

                    testthat::expect_error(assert_ts(mts_A), regexp = "Variable 'mts_A': Must be of type 'atomic vector'")
                }
            }
        }
    }
})


### Mauvaise fréquence ---------------------------------------------------------

testthat::test_that("Result FALSE expected with wrong frequency", {
    for (wrong_freq in weird_frequency) {
        for (typeA in list_type) {
            for (startA in list_start) {
                for (lenA in list_len[-c(1L, 9L)]) {
                    ts_A <- create_random_ts(
                        type = typeA, len = lenA,
                        frequency = wrong_freq,
                        start = startA
                    )
                    testthat::expect_error(
                        object = assert_ts(ts_A),
                        regexp = paste(
                            "Variable 'frequency_ts': Must be element of set",
                            "\\{'4','12'\\}|Variable 'frequency_ts': Must be",
                            "of type 'single integerish value'"
                        )
                    )
                }
            }
        }
    }
})


### Mauvais type ---------------------------------------------------------------

testthat::test_that("Result FALSE expected with wrong type of ts", {
    for (wrong_ts in wrong_type_ts) {
        testthat::expect_error(assert_ts(wrong_ts), regexp = "Variable 'wrong_ts': Must be of type 'atomic vector'")
    }
})


### Mauvais objet R ------------------------------------------------------------

testthat::test_that("Result FALSE expected with wrong object R", {
    for (wrong_ts in object_bank_R) {
        testthat::expect_error(assert_ts(wrong_ts))
    }
})
