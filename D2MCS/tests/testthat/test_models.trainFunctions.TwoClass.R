testthat::test_that("TwoClass: initialize function works", {
  testthat::expect_is(TwoClass$new(method = "cv",
                                   number = 10,
                                   savePredictions = "final",
                                   classProbs = TRUE,
                                   allowParallel = TRUE,
                                   verboseIter = FALSE),
                      "TwoClass")
})

testthat::test_that("TwoClass: create function works", {

  method <- "cv"
  number <- 10
  savePredictions <- "final"
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- FALSE

  twoClass <- TwoClass$new(method = method,
                           number = number,
                           savePredictions = savePredictions,
                           classProbs = classProbs,
                           allowParallel = allowParallel,
                           verboseIter = verboseIter)

  summaryFunction <- UseProbability$new()
  search.method <- "grid"
  class.probs <- NULL

  twoClass$create(summaryFunction = summaryFunction,
                  search.method = search.method,
                  class.probs = class.probs)

  tr <- caret::trainControl(method = "cv",
                            number = 10,
                            savePredictions = "final",
                            classProbs = TRUE,
                            summaryFunction = UseProbability$new()$execute,
                            search = "grid",
                            allowParallel = TRUE,
                            verboseIter = FALSE)

  testthat::expect_equal(twoClass$getTrFunction(), tr)
  testthat::expect_equal(twoClass$getMeasures(), summaryFunction$getMeasures())
})

testthat::test_that("TwoClass: create function checks parameter", {

  method <- "cv"
  number <- 10
  savePredictions <- "final"
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- FALSE

  twoClass <- TwoClass$new(method = method, number = number, savePredictions = savePredictions,
                           classProbs = classProbs, allowParallel = allowParallel, verboseIter = verboseIter)

  summaryFunction <- UseProbability$new()
  search.method <- "grid"
  class.probs <- NULL

  testthat::expect_error(twoClass$create(summaryFunction = NULL,
                                         search.method = search.method,
                                         class.probs = class.probs),
                         "[TwoClass][FATAL] SummaryFunction parameter must be defined as 'SummaryFunction' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_message(twoClass$create(summaryFunction = summaryFunction,
                                           search.method = NULL,
                                           class.probs = class.probs),
                         "[TwoClass][WARNING] Invalid search method. Only 'random' or 'grid' search method are available. Assuming grid method",
                         fixed = TRUE)

  testthat::expect_message(twoClass$create(summaryFunction = summaryFunction,
                                           search.method = "wrong",
                                           class.probs = class.probs),
                           "[TwoClass][WARNING] Invalid search method. Only 'random' or 'grid' search method are available. Assuming grid method",
                           fixed = TRUE)
})

testthat::test_that("TwoClass: getTrFunction function works", {

  method <- "cv"
  number <- 10
  savePredictions <- "final"
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- FALSE

  twoClass <- TwoClass$new(method = method,
                           number = number,
                           savePredictions = savePredictions,
                           classProbs = classProbs,
                           allowParallel = allowParallel,
                           verboseIter = verboseIter)

  summaryFunction <- UseProbability$new()
  search.method <- "grid"
  class.probs <- NULL

  twoClass$create(summaryFunction = summaryFunction,
                  search.method = search.method,
                  class.probs = class.probs)

  tr <- caret::trainControl(method = "cv",
                            number = 10,
                            savePredictions = "final",
                            classProbs = TRUE,
                            summaryFunction = UseProbability$new()$execute,
                            search = "grid",
                            allowParallel = TRUE,
                            verboseIter = FALSE)

  testthat::expect_equal(twoClass$getTrFunction(), tr)
})

testthat::test_that("TwoClass: getTrFunction function checks parameter", {

  method <- "cv"
  number <- 10
  savePredictions <- "final"
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- FALSE

  twoClass <- TwoClass$new(method = method,
                           number = number,
                           savePredictions = savePredictions,
                           classProbs = classProbs,
                           allowParallel = allowParallel,
                           verboseIter = verboseIter)

  testthat::expect_equal(twoClass$getTrFunction(), NULL)
  testthat::expect_message(twoClass$getTrFunction(),
                           "[TwoClass][WARNING] TrainFunction is not created. Execute create method first. Task not performed",
                           fixed = TRUE)



})

testthat::test_that("TwoClass: setClassProbs function works (after create)", {

  method <- "cv"
  number <- 10
  savePredictions <- "final"
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- FALSE

  twoClass <- TwoClass$new(method = method,
                           number = number,
                           savePredictions = savePredictions,
                           classProbs = classProbs,
                           allowParallel = allowParallel,
                           verboseIter = verboseIter)

  summaryFunction <- UseProbability$new()
  search.method <- "grid"
  class.probs <- NULL

  twoClass$create(summaryFunction = summaryFunction,
                  search.method = search.method,
                  class.probs = class.probs)

  class.probs.true <- TRUE

  twoClass$setClassProbs(class.probs = class.probs.true)
  testthat::expect_equal(twoClass$getTrFunction()$classProbs, class.probs.true)

  class.probs.false <- FALSE

  twoClass$setClassProbs(class.probs = class.probs.false)
  testthat::expect_equal(twoClass$getTrFunction()$classProbs, class.probs.false)
})

testthat::test_that("TwoClass: setClassProbs function checks parameter", {

  method <- "cv"
  number <- 10
  savePredictions <- "final"
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- FALSE

  twoClass <- TwoClass$new(method = method,
                           number = number,
                           savePredictions = savePredictions,
                           classProbs = classProbs,
                           allowParallel = allowParallel,
                           verboseIter = verboseIter)

  testthat::expect_message(twoClass$setClassProbs(class.probs = NULL),
                           "[TwoClass][WARNING] Class probabilities parameter is null or erroneous. Task not performed",
                           fixed = TRUE)

  testthat::expect_message(twoClass$setClassProbs(class.probs = list()),
                           "[TwoClass][WARNING] Class probabilities parameter is null or erroneous. Task not performed",
                           fixed = TRUE)

  class.probs <- TRUE

  testthat::expect_message(twoClass$setClassProbs(class.probs = class.probs),
                           "[TwoClass][WARNING] TrainFunction is not created. Execute create method first. Task not performed",
                           fixed = TRUE)
})

testthat::test_that("TwoClass: getMeasures function works", {

  method <- "cv"
  number <- 10
  savePredictions <- "final"
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- FALSE

  twoClass <- TwoClass$new(method = method,
                           number = number,
                           savePredictions = savePredictions,
                           classProbs = classProbs,
                           allowParallel = allowParallel,
                           verboseIter = verboseIter)

  summaryFunction <- UseProbability$new()
  search.method <- "grid"
  class.probs <- NULL

  twoClass$create(summaryFunction = summaryFunction,
                  search.method = search.method,
                  class.probs = class.probs)

  testthat::expect_equal(twoClass$getMeasures(), summaryFunction$getMeasures())
})

testthat::test_that("TwoClass: getType function works", {

  method <- "cv"
  number <- 10
  savePredictions <- "final"
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- FALSE

  twoClass <- TwoClass$new(method = method,
                           number = number,
                           savePredictions = savePredictions,
                           classProbs = classProbs,
                           allowParallel = allowParallel,
                           verboseIter = verboseIter)

  testthat::expect_equal(twoClass$getType(), "Bi-Class")
})

testthat::test_that("TwoClass: setSummaryFunction function works (after create)", {

  method <- "cv"
  number <- 10
  savePredictions <- "final"
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- FALSE

  twoClass <- TwoClass$new(method = method,
                           number = number,
                           savePredictions = savePredictions,
                           classProbs = classProbs,
                           allowParallel = allowParallel,
                           verboseIter = verboseIter)

  summaryFunction <- UseProbability$new()
  search.method <- "grid"
  class.probs <- NULL

  twoClass$create(summaryFunction = summaryFunction,
                  search.method = search.method,
                  class.probs = class.probs)

  twoClass$setSummaryFunction(summaryFunction = NoProbability$new())

  testthat::expect_false(is.null(twoClass$getTrFunction()$summaryFunction))
  testthat::expect_equal(twoClass$getTrFunction()$summaryFunction, NoProbability$new()$execute)
})

testthat::test_that("TwoClass: setSummaryFunction function checks parameter", {

  method <- "cv"
  number <- 10
  savePredictions <- "final"
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- FALSE

  twoClass <- TwoClass$new(method = method,
                           number = number,
                           savePredictions = savePredictions,
                           classProbs = classProbs,
                           allowParallel = allowParallel,
                           verboseIter = verboseIter)

  testthat::expect_message(twoClass$setSummaryFunction(summaryFunction = NULL),
                           "[TwoClass][WARNING] SummaryFunction parameter is null or incorrect type. Task not performed",
                           fixed = TRUE)

  testthat::expect_message(twoClass$setSummaryFunction(summaryFunction = list()),
                           "[TwoClass][WARNING] SummaryFunction parameter is null or incorrect type. Task not performed",
                           fixed = TRUE)

  testthat::expect_message(twoClass$setSummaryFunction(summaryFunction = UseProbability$new()),
                           "[TwoClass][WARNING] TrainFunction is not created. Execute create method first. Task not performed",
                           fixed = TRUE)
})
