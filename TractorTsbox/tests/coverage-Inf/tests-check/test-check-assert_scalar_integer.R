# Initialisation ---------------------------------------------------------------

set.seed(2036L)


# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for integer", {

    list_integer <- c(list_lag, list_len, create_random_type("integer", len = 10L))

    for (k in list_integer) {
        res <- testthat::expect_silent(assert_scalar_integer(x = k))
        testthat::expect_identical(res, k)
    }
})

testthat::test_that("good result for double without warning", {

    list_double <- as.double(c(list_lag, list_len, create_random_type("integer", len = 10L)))

    for (k in list_double) {
        res <- testthat::expect_silent(assert_scalar_integer(x = k, warn = FALSE))
        testthat::expect_identical(res, as.integer(k))
    }
})


# Tests positifs avec warning --------------------------------------------------

testthat::test_that("warning for integer date", {

    list_double <- as.double(c(list_lag, list_len, create_random_type("integer", len = 10L)))

    for (k in list_double) {
        testthat::expect_warning(
            {res1 <- assert_scalar_integer(x = k, warn = TRUE)},
            regexp = double_instead_of_integer
        )
        testthat::expect_identical(res1, as.integer(k))

        testthat::expect_warning(
            {res2 <- assert_scalar_integer(x = k)},
            regexp = double_instead_of_integer
        )
        testthat::expect_identical(res2, as.integer(k))
    }

})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("miscellaneous integer x are not allowed", {
    for (wrong_integer in c(list_wrong_date_ts,
                            object_bank_R[-10L],
                            rnorm(10L))) {
        testthat::expect_error(
            assert_scalar_integer(x = wrong_integer, warn = FALSE)
        )
    }
})
