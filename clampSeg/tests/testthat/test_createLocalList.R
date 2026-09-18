context("createLocalList")

test_that("everything works when arguments are given", {
  testy <- rnorm(50)
  testfilter <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 1e4)
  testlengths <- 3:4
  
  testlocalList <- createLocalList(filter = testfilter, method = "LR", lengths = testlengths)
  expect_is(testlocalList, "localList")
  expect_true(is.list(testlocalList))
  expect_identical(length(testlocalList), length(testlengths))
  expect_identical(attr(testlocalList, "method"), "LR")
  expect_identical(attr(testlocalList, "filter"), testfilter)
  expect_identical(attr(testlocalList, "lengths"), testlengths)
  
  expect_error(stepR::critVal(50L, alpha = 0.1, stat = teststat, family = "LR",
                              filter = testfilter, lengths = testlengths, localList = 1))
  expect_error(stepR::computeStat(testy, family = "LR", filter = testfilter, lengths = testlengths,
                                  localList = list()))

  expect_identical(stepR::computeStat(testy, family = "LR", filter = testfilter, lengths = testlengths),
                   stepR::computeStat(testy, family = "LR", filter = testfilter, lengths = testlengths,
                                      localList = testlocalList))
  
  expect_identical(stepR::monteCarloSimulation(family = "LR", n = 50L, r = 2L, filter = testfilter,
                                               lengths = testlengths, output = "maximum"),
                   stepR::monteCarloSimulation(family = "LR", n = 50L, r = 2L, filter = testfilter,
                                               lengths = testlengths, output = "maximum",
                                               localList = testlocalList))
  
  testlocalList <- createLocalList(filter = testfilter, method = "LR")
  expect_identical(stepR::monteCarloSimulation(family = "LR", n = 35L, r = 2L, filter = testfilter),
                   stepR::monteCarloSimulation(family = "LR", n = 35L, r = 2L, filter = testfilter,
                                               localList = testlocalList))
  
  teststat <- stepR::monteCarloSimulation(35L, r = 10L, family = "LR", filter = testfilter)
  expect_identical(stepR::critVal(35L, alpha = 0.1, stat = teststat,  family = "LR", filter = testfilter),
                   stepR::critVal(35L, alpha = 0.1, stat = teststat,  family = "LR", filter = testfilter,
                                  localList = testlocalList))
  expect_equal(stepR::critVal(35L, alpha = 0.1, stat = teststat,  family = "LR", filter = testfilter),
               stepR::critVal(35L, alpha = 0.1, r = 10, family = "LR", filter = testfilter,
                              localList = testlocalList))
  
  testfilter <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 1e4)  
  testlocalList <- clampSeg::createLocalList(filter = testfilter, method = "LR")
  teststat <- stepR::monteCarloSimulation(35L, r = 10L, family = "LR", filter = testfilter)
  expect_equal(stepR::critVal(35L, alpha = 0.1, stat = teststat,  family = "LR", filter = testfilter,
                              options = list(load = list())),
               stepR::critVal(35L, alpha = 0.1, r = 10, family = "LR", filter = testfilter,
                              localList = testlocalList,
                              options = list(load = list(), simulation = "matrix")))
  
  testlocalList <- createLocalList(filter = testfilter, method = "2Param", lengths = testlengths)
  expect_is(testlocalList, "localList")
  expect_true(is.list(testlocalList))
  expect_identical(length(testlocalList), length(testlengths))
  expect_identical(attr(testlocalList, "method"), "2Param")
  expect_identical(attr(testlocalList, "filter"), testfilter)
  expect_identical(attr(testlocalList, "lengths"), testlengths)
  
  expect_identical(stepR::computeStat(testy, family = "2Param", filter = testfilter, lengths = testlengths),
                   stepR::computeStat(testy, family = "2Param", filter = testfilter, lengths = testlengths,
                                      localList = testlocalList))
  
  expect_identical(stepR::monteCarloSimulation(family = "2Param", n = 50L, r = 2L, filter = testfilter,
                                               lengths = testlengths, output = "maximum"),
                   stepR::monteCarloSimulation(family = "2Param", n = 50L, r = 2L, filter = testfilter,
                                               lengths = testlengths, output = "maximum",
                                               localList = testlocalList))
  
  expect_error(stepR::computeStat(testy, family = "2Param", filter = testfilter, lengths = testlengths,
                                  localList = 1))
  expect_error(stepR::computeStat(testy, family = "2Param", filter = testfilter, lengths = testlengths,
                                  localList = list()))
  expect_error(stepR::computeStat(testy, family = "2Param", filter = testfilter, lengths = testlengths,
                                  localList = unclass(testlocalList)))
})

test_that("filter is tested and works", {
  testy <- rnorm(50)
  testfilter <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 6, cutoff = 0.2), sr = 1e4)
  testlengths <- 5
  
  expect_error(createLocalList())
  expect_error(createLocalList(method = "LR", lengths = testlengths))
  expect_error(createLocalList(filter = list(test = 1), method = "LR", lengths = testlengths))
  expect_error(createLocalList(filter = unclass(testfilter), method = "LR", lengths = testlengths))
  
  testlocalList <- createLocalList(filter = testfilter, method = "LR", lengths = testlengths)
  expect_identical(attr(testlocalList, "filter"), testfilter)
  
  testfilter2 <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 1e4)
  expect_error(stepR::computeStat(testy, family = "LR", filter = testfilter2, lengths = testlengths,
                                  localList = testlocalList))
  expect_identical(stepR::computeStat(testy, family = "LR", filter = testfilter, lengths = testlengths),
                   stepR::computeStat(testy, family = "LR", filter = testfilter, lengths = testlengths,
                                      localList = testlocalList))
  
  testlocalList <- createLocalList(filter = testfilter, method = "2Param", lengths = testlengths)
  expect_error(stepR::computeStat(testy, family = "2Param", filter = testfilter2, lengths = testlengths,
                                  localList = testlocalList))
  expect_identical(stepR::computeStat(testy, family = "2Param", filter = testfilter, lengths = testlengths),
                   stepR::computeStat(testy, family = "2Param", filter = testfilter, lengths = testlengths,
                                      localList = testlocalList))
})

test_that("method is tested and works", {
  testy <- rnorm(50)
  testfilter <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.075), sr = 1e4)
  testlengths <- c(2, 7)
  
  expect_error(createLocalList(filter = testfilter, method = Inf, lengths = testlengths))
  expect_error(createLocalList(filter = testfilter, method = "s", lengths = testlengths))
  expect_error(createLocalList(filter = testfilter, method = "2L", lengths = testlengths))
  expect_error(createLocalList(filter = testfilter, method = "", lengths = testlengths))
  
  testlocalList <- createLocalList(filter = testfilter, method = "LR", lengths = testlengths)
  expect_error(stepR::computeStat(testy, family = "2Param", filter = testfilter, lengths = testlengths,
                                  localList = testlocalList))
  
  testlocalList <- createLocalList(filter = testfilter, lengths = testlengths)
  expect_error(stepR::computeStat(testy, family = "LR", filter = testfilter, lengths = testlengths,
                                  localList = testlocalList))
  expect_identical(suppressWarnings(stepR::computeStat(testy, family = "2Param", filter = testfilter, lengths = testlengths)),
                   suppressWarnings(stepR::computeStat(testy, family = "2Param", filter = testfilter, lengths = testlengths,
                                      localList = testlocalList)))
})

test_that("lengths is tested and works", {
  testy <- rnorm(50)
  testfilter <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 6, cutoff = 0.2), sr = 1e4)
  testlengths <- c(3, 8:9)
  
  testlocalList <- createLocalList(filter = testfilter, method = "LR", lengths = testlengths)
  
  expect_error(createLocalList(filter = testfilter, method = "LR", lengths = NULL))
  expect_error(createLocalList(filter = testfilter, method = "LR", lengths = c(3, "s", 4)))
  expect_error(createLocalList(filter = testfilter, method = "LR", lengths = 0L))
  expect_error(createLocalList(filter = testfilter, method = "LR", lengths = c(-1L, 10L)))
  expect_error(createLocalList(filter = testfilter, method = "LR", lengths = c(5L, Inf)))
  
  expect_identical(createLocalList(filter = testfilter, method = "LR", lengths = c(9, 3, 8)), testlocalList)
  expect_identical(createLocalList(filter = testfilter, method = "LR", lengths = c(3.2, 8.1, 9.3)), testlocalList)
  expect_warning(ret <- createLocalList(filter = testfilter, method = "LR", lengths = c(3.2, 8.1, 9.1, 9.3, 3.6)))
  expect_identical(ret, testlocalList)
  expect_identical(createLocalList(filter = testfilter, method = "LR", lengths = 1:20),
                   createLocalList(filter = testfilter, method = "LR"))
  
  expect_error(stepR::computeStat(testy, family = "LR", filter = testfilter, lengths = c(4, 5),
                                  localList = testlocalList))
  expect_identical(stepR::computeStat(testy, family = "LR", filter = testfilter, lengths = testlengths),
                   stepR::computeStat(testy, family = "LR", filter = testfilter, lengths = testlengths,
                                      localList = testlocalList))
  
  testlocalList <- createLocalList(filter = testfilter, method = "2Param", lengths = testlengths)
  expect_error(stepR::computeStat(testy, family = "2Param", filter = testfilter,
                                  localList = testlocalList))
  expect_identical(stepR::computeStat(testy, family = "2Param", filter = testfilter, lengths = testlengths),
                   stepR::computeStat(testy, family = "2Param", filter = testfilter, lengths = testlengths,
                                      localList = testlocalList))
})

test_that("it is compatibel with other arguments when passed to stepR functions", {
  testn <- 100L
  testfilter <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 1)
  testy <- lowpassFilter::randomGenerationMA(n = testn, filter = testfilter, seed = "no")
  testlengths <- c(1, 2, 4, 8, 16)
  
  testlocalList <- createLocalList(filter = testfilter, method = "LR", lengths = testlengths)
  
  testfit <-  stepR::stepblock(c(0, 1), leftEnd = c(0, 50 / testfilter$sr) - 10,
                               rightEnd = c(50 / testfilter$sr, testn / testfilter$sr) -10, x0 = -10)
  
  expect_identical(stepR::computeStat(testy, family = "LR", filter = testfilter, fit = testfit, 
                                      intervalSystem = "dyaLen", penalty = "sqrt", nq = 120L,
                                      thresholdLongSegment = 8L, localValue = mean, startTime = -10,
                                      regularization = c(1, 0.5),
                                      suppressWarningNoDeconvolution = TRUE),
                   stepR::computeStat(testy, family = "LR", filter = testfilter, fit = testfit,
                                      intervalSystem = "dyaLen", penalty = "sqrt", nq = 120L,
                                      thresholdLongSegment = 8L, localValue = mean, startTime = -10,
                                      regularization = c(1, 0.5),
                                      suppressWarningNoDeconvolution = TRUE, localList = testlocalList))
  
  expect_identical(stepR::monteCarloSimulation(family = "LR", n = 35L, r = 2L, filter = testfilter, seed = 34L,
                                               intervalSystem = "dyaLen", rand.gen = function(data) rnorm(data$n),
                                               thresholdLongSegment = 5L, localValue = mean, startTime = -10,
                                               regularization = c(0.5), suppressWarningNoDeconvolution = TRUE),
                   stepR::monteCarloSimulation(family = "LR", n = 35L, r = 2L, filter = testfilter, seed = 34L,
                                               intervalSystem = "dyaLen", rand.gen = function(data) rnorm(data$n),
                                               thresholdLongSegment = 5L, localValue = mean, startTime = -10,
                                               suppressWarningNoDeconvolution = TRUE, regularization = c(0.5),
                                               localList = testlocalList))
  
  teststat <- stepR::monteCarloSimulation(n = 100L, r = 10L, family = "LR", filter = testfilter)
  expect_equal(stepR::critVal(alpha = 0.125, n = 80L, stat = teststat, nq = 100L, family = "LR",
                              filter = testfilter, intervalSystem = "all", lengths = testlengths,
                              thresholdLongSegment = 5L, localValue = mean, startTime = -10,
                              regularization = c(0.5),
                              penalty = "sqrt", output = "value"),
               stepR::critVal(alpha = 0.125, n = 80L, stat = teststat, nq = 100L, family = "LR",
                              filter = testfilter, intervalSystem = "all", lengths = testlengths,
                              thresholdLongSegment = 5L, localValue = mean, startTime = -10,
                              regularization = c(0.5),
                              penalty = "sqrt", output = "value", localList = testlocalList))
  
  expect_equal(stepR::critVal(alpha = 0.075, n = 90L, stat = teststat, nq = 100L, family = "LR",
                              filter = testfilter, intervalSystem = "all", lengths = testlengths,
                              penalty = "log", output = "value", startTime = -10),
               stepR::critVal(alpha = 0.075, n = 90L, r = 10L, nq = 100L, family = "LR",
                              filter = testfilter, intervalSystem = "all", lengths = testlengths,
                              options = list(simulation = "vectorIncreased", save = list(), load = list()),
                              penalty = "log", output = "value", startTime = -10, localList = testlocalList))
  
  

  testy <- lowpassFilter::randomGenerationMA(n = testn, filter = testfilter, seed = "no")  
  testfit <-  stepR::stepblock(c(0, 1, 0), leftEnd = c(0, 50 / testfilter$sr, 53 / testfilter$sr),
                               rightEnd = c(50 / testfilter$sr, 53 / testfilter$sr, testn / testfilter$sr), x0 = 0)
  
  compare <- stepR::computeStat(y = testy, family = "LR", filter = testfilter, fit = testfit, lengths = testlengths)
  expect_identical(stepR::computeStat(y = testy, filter = testfilter, family = "LR", fit = testfit,
                                      lengths = testlengths, localList = testlocalList), compare)
  expect_identical(stepR::computeStat(y = testy, filter = testfilter, family = "LR", fit = testfit,
                                      lengths = testlengths, localList = testlocalList,
                                      output = "maximum"), compare$maximum)
  expect_identical(stepR::computeStat(y = testy, filter = testfilter, family = "LR", fit = testfit,
                                      lengths = testlengths, localList = testlocalList,
                                      output = "vector"), compare$stat)
  
  testlengths <- c(3, 5)
  testlocalList <- createLocalList(filter = testfilter, method = "2Param", lengths = testlengths)

  expect_identical(stepR::computeStat(testy, family = "2Param", filter = testfilter, fit = testfit,
                                      intervalSystem = "all", penalty = "log", nq = 120L, lengths = testlengths,
                                      thresholdLongSegment = 13L, localValue = function(x) 1,
                                      localVar = sd, regularization = c(0.8), suppressWarningNoDeconvolution = TRUE),
                   stepR::computeStat(testy, family = "2Param", filter = testfilter, fit = testfit,
                                      penalty = "log", nq = 120L, lengths = testlengths,
                                      thresholdLongSegment = 13L, localValue = function(x) 1,
                                      localVar = sd, regularization = c(0.8), suppressWarningNoDeconvolution = TRUE,
                                      localList = testlocalList))
  
  expect_identical(stepR::monteCarloSimulation(family = "2Param", n = 75L, r = 2L, filter = testfilter, seed = 14L,
                                               output = "maximum", penalty = "log", lengths = testlengths,
                                               intervalSystem = "all", rand.gen = function(data) rnorm(data$n) + 1,
                                               thresholdLongSegment = 7L, localValue = function(x) mean(x) - 1,
                                               suppressWarningNoDeconvolution = TRUE),
                   stepR::monteCarloSimulation(family = "2Param", n = 75L, r = 2L, filter = testfilter, seed = 14L,
                                               output = "maximum", penalty = "log", lengths = testlengths,
                                               rand.gen = function(data) rnorm(data$n) + 1,
                                               thresholdLongSegment = 7L, localValue = function(x) mean(x) - 1,
                                               suppressWarningNoDeconvolution = TRUE,
                                               localList = testlocalList))
  
  teststat <- stepR::monteCarloSimulation(n = 100L, r = 2L, family = "2Param", filter = testfilter,
                                          output = "maximum", lengths = testlengths, localList = testlocalList,
                                          penalty = "log")
  expect_equal(stepR::critVal(alpha = 0.33, n = 80L, r = 2L, nq = 100L, family = "2Param",
                              filter = testfilter, intervalSystem = "all", lengths = testlengths,
                              penalty = "log"),
               stepR::critVal(alpha = 0.335, n = 80L, stat = teststat, nq = 100L, family = "2Param",
                              filter = testfilter, intervalSystem = "all", lengths = testlengths,
                              penalty = "log", localList = testlocalList))
  
  
  testn <- 100L
  testfilter <- lowpassFilter::lowpassFilter(type = "bessel", param = list(pole = 4, cutoff = 0.1), sr = 1e4)
  testy <- lowpassFilter::randomGenerationMA(n = testn, filter = testfilter, seed = "no")
  testfit <-  stepR::stepblock(0, leftEnd = 0, rightEnd = testn / testfilter$sr, x0 = 0)
  testlengths <- c(1, 2, 4, 8, 16)
  
  testlocalList <- createLocalList(filter = testfilter, method = "LR", lengths = testlengths)
  teststat <- stepR::monteCarloSimulation(n = 100L, r = 10L, family = "LR", filter = testfilter)
  expect_equal(stepR::critVal(alpha = 0.075, n = 90L, stat = teststat, nq = 100L, family = "LR",
                              filter = testfilter, intervalSystem = "all", lengths = testlengths,
                              penalty = "log", output = "value"),
               stepR::critVal(alpha = 0.075, n = 90L, r = 10L, nq = 100L, family = "LR",
                              filter = testfilter, intervalSystem = "all", lengths = testlengths,
                              options = list(simulation = "matrixIncreased", save = list(), load = list()),
                              penalty = "log", output = "value", localList = testlocalList))
  
  expect_equal(stepR::critVal(alpha = 0.175, n = 100L, stat = teststat, family = "LR",
                              filter = testfilter, intervalSystem = "all", lengths = testlengths,
                              penalty = "sqrt", output = "value"),
               stepR::critVal(alpha = 0.175, n = 100L, r = 10L, family = "LR",
                              filter = testfilter, intervalSystem = "all", lengths = testlengths,
                              options = list(simulation = "matrix", save = list(), load = list()),
                              penalty = "sqrt", output = "value", localList = testlocalList))
  
  
  expect_equal(stepR::critVal(alpha = 0.075, n = 90L, stat = teststat, nq = 100L, family = "LR",
                              filter = testfilter, intervalSystem = "all", lengths = testlengths,
                              output = "vector"),
               stepR::critVal(alpha = 0.075, n = 90L, r = 10L, nq = 100L, family = "LR",
                              filter = testfilter, intervalSystem = "all", lengths = testlengths,
                              options = list(simulation = "matrixIncreased", save = list(), load = list()),
                              output = "vector", localList = testlocalList))
  
  expect_equal(stepR::critVal(alpha = 0.175, n = 100L, stat = teststat, family = "LR",
                              filter = testfilter, intervalSystem = "all", lengths = testlengths,
                              output = "vector"),
               stepR::critVal(alpha = 0.175, n = 100L, r = 10L, family = "LR",
                              filter = testfilter, intervalSystem = "all", lengths = testlengths,
                              options = list(simulation = "matrix", save = list(), load = list()),
                              output = "vector", localList = testlocalList))
})

