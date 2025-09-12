testthat::test_that("GenericClusteringStrategy: initialize function works", {

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  testthat::expect_silent(GenericClusteringStrategy$new(subset = subset,
                                                 heuristic = heuristic,
                                                 description = description,
                                                 configuration = configuration))
})

testthat::test_that("GenericClusteringStrategy: initialize function checks parameter type", {

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- MCCHeuristic$new()
  description <- NULL
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(GenericClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[GenericClusteringStrategy][FATAL] Strategy description parameter must be defined as 'character' type. Aborting...",
                         fixed = TRUE)

  subset <- NULL
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(GenericClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[GenericClusteringStrategy][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- NULL
  description <- "example"
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(GenericClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[GenericClusteringStrategy][FATAL] Heuristics is not correct (must inherit from 'GenericHeuristic' class). Aborting...",
                         fixed = TRUE)

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- list()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(GenericClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[GenericClusteringStrategy][FATAL] Adequate heuristics not found (must inherit from 'GenericHeuristic' class). Aborting...",
                         fixed = TRUE)

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- NULL

  testthat::expect_error(GenericClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[GenericClusteringStrategy][FATAL] Configuration parameter must be inherit from 'StrategyConfiguration' class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("GenericClusteringStrategy: getDescription function works", {

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_equal(strategy$getDescription(), description)
})

testthat::test_that("GenericClusteringStrategy: getHeuristic function works", {

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_equal(strategy$getHeuristic(), list(heuristic))
})

testthat::test_that("GenericClusteringStrategy: getConfiguration function works", {

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_equal(strategy$getConfiguration(), configuration)
})

testthat::test_that("GenericClusteringStrategy: getBestClusterDistribution function works", {

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_null(strategy$getBestClusterDistribution())
})

testthat::test_that("GenericClusteringStrategy: getUnclustered function works", {

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_null(strategy$getUnclustered())
})

testthat::test_that("GenericClusteringStrategy: execute function works", {

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$execute(verbose = TRUE),
                         "[GenericClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("GenericClusteringStrategy: getDistribution function works", {

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$getDistribution(),
                         "[GenericClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("GenericClusteringStrategy: createTrain function works", {

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$createTrain(subset = subset),
                         "[GenericClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("GenericClusteringStrategy: plot function works", {

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$plot(),
                         "[GenericClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("GenericClusteringStrategy: saveCSV function works", {

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

  subset <- data$createSubset(num.folds = c(1, 2),
                              class.index = "Class",
                              positive.class = "1")

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$saveCSV(dir.path = "example", "example"),
                         "[GenericClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})
