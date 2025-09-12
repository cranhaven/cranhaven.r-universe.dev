testthat::test_that("SimpleStrategy: initialize function works", {

  set.seed(1234)
  file.path <-  file.path("resourceFiles",
                          "data",
                          "hcc-data-complete-balanced.csv")

  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  data$createPartitions(num.folds = 4, class.balance = "Class")

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  heuristic <- ChiSquareHeuristic$new()
  configuration <- StrategyConfiguration$new()

  testthat::expect_is(SimpleStrategy$new(subset.cluster, heuristic, configuration),
                      "SimpleStrategy")
})

testthat::setup({
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "outputs"),
                                winslash = "/",
                                mustWork = FALSE))) {
    dir.create(normalizePath(path = file.path(tempdir(),
                                              "outputs"),
                             winslash = "/",
                             mustWork = FALSE))
  }
})

testthat::test_that("SimpleStrategy works", {
  testthat::skip_if_not_installed("grDevices")

  set.seed(1234)
  file.path <-  file.path("resourceFiles",
                          "data",
                          "hcc-data-complete-balanced.csv")

  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  data$createPartitions(num.folds = 4, class.balance = "Class")

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  heuristic <- ChiSquareHeuristic$new()
  configuration <- StrategyConfiguration$new()

  strategy <- SimpleStrategy$new(subset.cluster, heuristic, configuration)

  capture.output(suppressWarnings(strategy$execute(verbose = TRUE)))

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")

  heuristic <- MCCHeuristic$new()
  configuration <- StrategyConfiguration$new()

  strategy <- SimpleStrategy$new(subset.cluster, heuristic, configuration)

  testthat::expect_error(strategy$getDistribution(),
                         "[SimpleStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(strategy$createTrain(subset = subset.cluster),
                         "[SimpleStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)
  testthat::expect_error(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                    "outputs",
                                                                                    "saveCSV"),
                                                                   winslash = "/",
                                                                   mustWork = FALSE)),
                         "[SimpleStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  capture.output(suppressWarnings(strategy$execute(verbose = TRUE)))

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")
  testthat::expect_is(strategy$getUnclustered(), "list")

  testthat::expect_equal(length(strategy$getDistribution()), 2)
  testthat::expect_equal(length(strategy$getDistribution(num.clusters = 1)), 2)
  testthat::expect_equal(length(strategy$getDistribution(num.clusters = 2)), 2)
  testthat::expect_equal(length(strategy$getDistribution(num.groups = 1)), 1)
  testthat::expect_equal(length(strategy$getDistribution(include.unclustered = TRUE)), 3)

  testthat::expect_is(strategy$createTrain(subset = subset.cluster),
                      "Trainset")
  testthat::expect_error(strategy$createTrain(subset = NULL),
                         "[SimpleStrategy][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

  grDevices::pdf(NULL)

  testthat::expect_equal(c("gg", "ggplot"),
                         class(strategy$plot(dir.path = NULL,
                                             file.name = NULL)))

  testthat::expect_message(strategy$plot(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                   "outputs",
                                                                                   "plots"),
                                                                  winslash = "/",
                                                                  mustWork = FALSE),
                                         file.name = "SimpleGenericClusteringStrategyPlot"),
                           "[SimpleStrategy][INFO] Plot has been succesfully saved at",
                           fixed = TRUE)

  testthat::expect_error(strategy$saveCSV(dir.path = NULL),
                         "[SimpleStrategy][FATAL] Path not defined. Aborting...",
                         fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputs",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE)),
                           "[SimpleStrategy][WARNING] File name not defined. Using 'MCCHeuristic.csv'",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputs",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE)),
                           "[SimpleStrategy][WARNING] Number of clusters not defined. Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputs",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE),
                                            num.clusters = 2),
                           "[SimpleStrategy][WARNING] Type of num.clusters not valid (must be NULL or list type). Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputs",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE),
                                            num.clusters = list(2:60)),
                           "[SimpleStrategy][WARNING] Number of clusters exceeds maximum number of clusters. Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputs",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE),
                                            num.clusters = list(0:3)),
                           "[SimpleStrategy][WARNING] Number of clusters exceeds the range of minimum and maximum number of clusters. Saving all cluster configurations",
                           fixed = TRUE)
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "outputs"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "outputs"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})
