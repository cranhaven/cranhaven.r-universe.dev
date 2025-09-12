testthat::test_that("SummaryFunction: initialize function works", {

  testthat::expect_is(SummaryFunction$new(measures = "example"),
                      "SummaryFunction")
})

testthat::test_that("SummaryFunction: initialize function checks parameter type", {

  testthat::expect_error(SummaryFunction$new(measures = NULL),
                         "[SummaryFunction][FATAL] Measures were not defined. Aborting...",
                         fixed = TRUE)
  })

testthat::test_that("SummaryFunction: execute function works", {

  testthat::expect_error(SummaryFunction$new(measures = "example")$execute(),
                         "[SummaryFunction][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("SummaryFunction: getMeasures function works", {

  testthat::expect_equal(SummaryFunction$new(measures = "example")$getMeasures(),
                         "example",
                         fixed = TRUE)
})
