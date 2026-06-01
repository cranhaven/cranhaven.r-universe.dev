
# Tests de résultats positifs --------------------------------------------------

testthat::test_that("good result for no error", {

    testthat::expect_silent(assert_expression(2 + 2))
    testthat::expect_silent(assert_expression(log(1)))

})


# Tests de résultats négatifs --------------------------------------------------

testthat::test_that("wrong result in case of warnings", {

    testthat::expect_error(assert_expression(expr = log(-1)))
    testthat::expect_error(assert_expression(warning("Un petit warning")))

})

testthat::test_that("wrong result in case of error", {

    testthat::expect_error(assert_expression(log("a")))
    testthat::expect_error(assert_expression(stop("Une petit erreur")))

})

testthat::test_that("wrong result in case of warnings and errors", {

    testthat::expect_error(assert_expression({
        warning("D'abord un warning")
        stop("Puis une erreur")
    }))

})
