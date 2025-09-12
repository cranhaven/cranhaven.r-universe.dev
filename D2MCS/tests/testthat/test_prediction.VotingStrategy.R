testthat::test_that("VotingStrategy: initialize function works", {

  testthat::expect_is(VotingStrategy$new(),
                      "VotingStrategy")
})

testthat::test_that("VotingStrategy: getVotingSchemes function works", {

  testthat::expect_null(VotingStrategy$new()$getVotingSchemes())
})

testthat::test_that("VotingStrategy: getMetrics function works", {

  testthat::expect_null(VotingStrategy$new()$getMetrics())
})

testthat::test_that("VotingStrategy: execute function works", {

  testthat::expect_error(VotingStrategy$new()$execute(predictions = NULL),
                         "[VotingStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("VotingStrategy: getName function works", {

  testthat::expect_equal(VotingStrategy$new()$getName(),
                         "VotingStrategy",
                         fixed = TRUE)
})
