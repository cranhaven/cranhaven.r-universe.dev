testthat::test_that("DependencyBasedStrategyConfiguration: initialize function works", {
  testthat::expect_is(DependencyBasedStrategyConfiguration$new(binaryCutoff = 0.6,
                                                               realCutoff = 0.6,
                                                               tiebreakMethod = "lfdc",
                                                               metric = "dep.tar"),
                      "DependencyBasedStrategyConfiguration")
})

testthat::test_that("DependencyBasedStrategyConfiguration: initialize function checks parameter type", {

  binaryCutoff <- 0.6
  realCutoff <- 0.6
  tiebreakMethod <- "lfdc"
  metric <- "dep.tar"

  testthat::expect_error(DependencyBasedStrategyConfiguration$new(binaryCutoff = NULL,
                                                                  realCutoff = realCutoff,
                                                                  tiebreakMethod = tiebreakMethod,
                                                                  metric = metric),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid binary cut-off value. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(DependencyBasedStrategyConfiguration$new(binaryCutoff = 1.5,
                                                                  realCutoff = realCutoff,
                                                                  tiebreakMethod = tiebreakMethod,
                                                                  metric = metric),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid binary cut-off value. Aborting...",
                         fixed = TRUE)

   testthat::expect_error(DependencyBasedStrategyConfiguration$new(binaryCutoff = binaryCutoff,
                                                                  realCutoff = NULL,
                                                                  tiebreakMethod = tiebreakMethod,
                                                                  metric = metric),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid real cut-off value. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(DependencyBasedStrategyConfiguration$new(binaryCutoff = binaryCutoff,
                                                                  realCutoff = 1.5,
                                                                  tiebreakMethod = tiebreakMethod,
                                                                  metric = metric),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid real cut-off value. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(DependencyBasedStrategyConfiguration$new(binaryCutoff = binaryCutoff,
                                                                  realCutoff = realCutoff,
                                                                  tiebreakMethod = NULL,
                                                                  metric = metric),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid real tiebreak method ('lfdc' or 'ltdc'). Aborting...",
                         fixed = TRUE)

  testthat::expect_error(DependencyBasedStrategyConfiguration$new(binaryCutoff = binaryCutoff,
                                                                  realCutoff = realCutoff,
                                                                  tiebreakMethod = 1,
                                                                  metric = metric),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid real tiebreak method ('lfdc' or 'ltdc'). Aborting...",
                         fixed = TRUE)

  testthat::expect_error(DependencyBasedStrategyConfiguration$new(binaryCutoff = binaryCutoff,
                                                                  realCutoff = realCutoff,
                                                                  tiebreakMethod = "a",
                                                                  metric = metric),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid real tiebreak method ('lfdc' or 'ltdc'). Aborting...",
                         fixed = TRUE)

  testthat::expect_error(DependencyBasedStrategyConfiguration$new(binaryCutoff = binaryCutoff,
                                                                  realCutoff = realCutoff,
                                                                  tiebreakMethod = tiebreakMethod,
                                                                  metric = NULL),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid metric value ('dep.fea' or 'dep.tar'). Aborting...",
                         fixed = TRUE)

  testthat::expect_error(DependencyBasedStrategyConfiguration$new(binaryCutoff = binaryCutoff,
                                                                  realCutoff = realCutoff,
                                                                  tiebreakMethod = tiebreakMethod,
                                                                  metric = 1),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid metric value ('dep.fea' or 'dep.tar'). Aborting...",
                         fixed = TRUE)

  testthat::expect_error(DependencyBasedStrategyConfiguration$new(binaryCutoff = binaryCutoff,
                                                                  realCutoff = realCutoff,
                                                                  tiebreakMethod = tiebreakMethod,
                                                                  metric = "a"),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid metric value ('dep.fea' or 'dep.tar'). Aborting...",
                         fixed = TRUE)

})

testthat::test_that("DependencyBasedStrategyConfiguration: minNumClusters function works", {

  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_type(configuration$minNumClusters(), "double")
})

testthat::test_that("DependencyBasedStrategyConfiguration: maxNumClusters function works", {

  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_equal(configuration$maxNumClusters(features = list(list(1, 2), list(1))), 2)
  testthat::expect_equivalent(suppressWarnings(configuration$maxNumClusters()), 3)

})

testthat::test_that("DependencyBasedStrategyConfiguration: getBinaryCutoff function works", {

  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_type(configuration$getBinaryCutoff(), "double")
})

testthat::test_that("DependencyBasedStrategyConfiguration: getRealCutoff function works", {

  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_type(configuration$getRealCutoff(), "double")
})

testthat::test_that("DependencyBasedStrategyConfiguration: setBinaryCutoff function works", {

  configuration <- DependencyBasedStrategyConfiguration$new()

  cutoff <- 0.2
  configuration$setBinaryCutoff(cutoff=cutoff)
  testthat::expect_equal(configuration$getBinaryCutoff(), cutoff)
})

testthat::test_that("DependencyBasedStrategyConfiguration: setRealCutoff function works", {

  configuration <- DependencyBasedStrategyConfiguration$new()

  cutoff <- 0.2
  configuration$setRealCutoff(cutoff=cutoff)
  testthat::expect_equal(configuration$getRealCutoff(), cutoff)
})

testthat::test_that("DependencyBasedStrategyConfiguration: setBinaryCutoff function checks parameter type", {

  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_error(configuration$setBinaryCutoff(cutoff=NULL),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid binary cut-off value. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(configuration$setBinaryCutoff(cutoff="a"),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid binary cut-off value. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(configuration$setBinaryCutoff(cutoff=1.1),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid binary cut-off value. Aborting...",
                         fixed = TRUE)

})

testthat::test_that("DependencyBasedStrategyConfiguration: setRealCutoff function checks parameter type", {

  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_error(configuration$setRealCutoff(cutoff=NULL),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid real cut-off value. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(configuration$setRealCutoff(cutoff="a"),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid real cut-off value. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(configuration$setRealCutoff(cutoff=1.1),
                         "[DependencyBasedStrategyConfiguration][FATAL] Invalid real cut-off value. Aborting...",
                         fixed = TRUE)
})
