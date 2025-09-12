testthat::test_that("D2MCS: initialize function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "D2MCS"),
                            winslash = "/",
                            mustWork = FALSE)
  num.core <- 1
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  testthat::expect_is(D2MCS$new(dir.path = dir.path,
                                num.core = num.core,
                                socket.type = socket.type,
                                outfile = outfile,
                                serialize = serialize),
                      "D2MCS")

  testthat::expect_is(D2MCS$new(dir.path = dir.path,
                                num.core = 2,
                                socket.type = socket.type,
                                outfile = outfile,
                                serialize = TRUE),
                      "D2MCS")
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "D2MCS"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "D2MCS"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("D2MCS: initialize function checks parameter type", {

  dir.path <- NULL
  num.core <- NULL
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  testthat::expect_error(D2MCS$new(dir.path = dir.path,
                                   num.core = num.core,
                                   socket.type = socket.type,
                                   outfile = outfile,
                                   serialize = serialize),
                         "[D2MCS][FATAL] Path to store ML models should be defined",
                         fixed = TRUE)

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "D2MCS"),
                            winslash = "/",
                            mustWork = FALSE)
  num.core <- 2
  socket.type <- "PSOCK"
  outfile <- normalizePath(path = file.path(tempdir(),
                                            "D2MCS",
                                            "outfile",
                                            "null"),
                           winslash = "/",
                           mustWork = FALSE)
  serialize <- NULL

  testthat::expect_message(D2MCS$new(dir.path = dir.path,
                                     num.core = num.core,
                                     socket.type = socket.type,
                                     outfile = outfile,
                                     serialize = serialize),
                           paste0("[D2MCS][INFO] Logs path not defined '", outfile, "' does not exist. Creating..."),
                           fixed = TRUE)

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "D2MCS"),
                            winslash = "/",
                            mustWork = FALSE)
  num.core <- 2
  socket.type <- "wrong"
  outfile <- NULL
  serialize <- NULL

  testthat::expect_message(D2MCS$new(dir.path = dir.path,
                                     num.core = num.core,
                                     socket.type = socket.type,
                                     outfile = outfile,
                                     serialize = serialize),
                           "[D2MCS][WARNING] Invalid socket type. Assuming 'PSOCK' cluster",
                           fixed = TRUE)

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "D2MCS"),
                            winslash = "/",
                            mustWork = FALSE)
  num.core <- 2
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  testthat::expect_message(D2MCS$new(dir.path = dir.path,
                                     num.core = num.core,
                                     socket.type = socket.type,
                                     outfile = outfile,
                                     serialize = serialize),
                           "[D2MCS][WARNING] Invalid serialization option. Assuming not serialization",
                           fixed = TRUE)
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "D2MCS"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "D2MCS"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("D2MCS: train function works", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("klaR")
  testthat::skip_if_not_installed("e1071")
  testthat::skip_if_not_installed("ranger")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("MASS")
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

  heuristic <- MCCHeuristic$new()
  configuration <- StrategyConfiguration$new()

  strategy <- SimpleStrategy$new(subset = subset.cluster,
                                 heuristic = heuristic,
                                 configuration = configuration)

  capture.output(suppressWarnings(strategy$execute(verbose = TRUE)))

  train.set <- strategy$createTrain(subset = subset.cluster)

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "D2MCS"),
                            winslash = "/",
                            mustWork = FALSE)
  num.core <- 1
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  d2mcs <- D2MCS$new(dir.path = dir.path,
                     num.core = num.core,
                     socket.type = socket.type,
                     outfile = outfile,
                     serialize = serialize)

  train.function <- TwoClass$new(method = "cv", number = 10, savePredictions = "final",
                                 classProbs = TRUE, allowParallel = TRUE, verboseIter = FALSE)
  num.clusters <- NULL
  ex.classifiers <- c("nb", "ranger", "lda", "lda2")
  ig.classifiers <- c()
  metrics <- c("MCC", "PPV")
  saveAllModels <- FALSE

  testthat::expect_is(suppressWarnings(d2mcs$train(train.set = train.set,
                                                   train.function = train.function,
                                                   num.clusters = num.clusters,
                                                   ex.classifiers = ex.classifiers,
                                                   ig.classifiers = ig.classifiers,
                                                   metrics = metrics,
                                                   saveAllModels = saveAllModels)),
                      "TrainOutput")
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "D2MCS"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "D2MCS"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("D2MCS: train function checks parameter types", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("klaR")
  testthat::skip_if_not_installed("e1071")
  testthat::skip_if_not_installed("ranger")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("MASS")
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

  heuristic <- MCCHeuristic$new()
  configuration <- StrategyConfiguration$new()

  strategy <- SimpleStrategy$new(subset = subset.cluster,
                                 heuristic = heuristic,
                                 configuration = configuration)

  capture.output(suppressWarnings(strategy$execute(verbose = TRUE)))

  train.set <- strategy$createTrain(subset = subset.cluster)

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "D2MCS"),
                            winslash = "/",
                            mustWork = FALSE)
  num.core <- 1
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  d2mcs <- D2MCS$new(dir.path = dir.path,
                     num.core = num.core,
                     socket.type = socket.type,
                     outfile = outfile,
                     serialize = serialize)

  train.function <- TwoClass$new(method = "cv", number = 10, savePredictions = "final",
                                 classProbs = TRUE, allowParallel = TRUE, verboseIter = FALSE)
  num.clusters <- NULL
  model.recipe <- DefaultModelFit$new()
  ex.classifiers <- c("nb", "ranger", "lda", "lda2")
  ig.classifiers <- c()
  metrics <- c("MCC", "PPV")
  saveAllModels <- FALSE

  testthat::expect_error(d2mcs$train(train.set = NULL,
                                     train.function = train.function,
                                     num.clusters = num.clusters,
                                     model.recipe = model.recipe,
                                     ex.classifiers = ex.classifiers,
                                     ig.classifiers = ig.classifiers,
                                     metrics = metrics,
                                     saveAllModels = saveAllModels),
                         "[D2MCS][FATAL] Train set parameter must be defined as 'Trainset' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(d2mcs$train(train.set = train.set,
                                     train.function = NULL,
                                     num.clusters = num.clusters,
                                     model.recipe = model.recipe,
                                     ex.classifiers = ex.classifiers,
                                     ig.classifiers = ig.classifiers,
                                     metrics = metrics,
                                     saveAllModels = saveAllModels),
                         "[D2MCS][FATAL] Train function parameter must be defined as 'TrainFunction' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_message(suppressWarnings(d2mcs$train(train.set = train.set,
                                                        train.function = train.function,
                                                        num.clusters = NULL,
                                                        model.recipe = model.recipe,
                                                        ex.classifiers = ex.classifiers,
                                                        ig.classifiers = ig.classifiers,
                                                        metrics = metrics,
                                                        saveAllModels = saveAllModels)),
                           "[D2MCS][WARNING] Number of clusters not set (must be numeric or vector). Using all clusters",
                           fixed = TRUE)

  testthat::expect_message(suppressWarnings(d2mcs$train(train.set = train.set,
                                                        train.function = train.function,
                                                        num.clusters = num.clusters,
                                                        model.recipe = NULL,
                                                        ex.classifiers = ex.classifiers,
                                                        ig.classifiers = ig.classifiers,
                                                        metrics = metrics,
                                                        saveAllModels = saveAllModels)),
                           "[D2MCS][WARNING] Model fit must inherit from 'GenericModelFit' type. Using 'DefaultModelFit' class.",
                           fixed = TRUE)

  testthat::expect_message(suppressWarnings(d2mcs$train(train.set = train.set,
                                                        train.function = train.function,
                                                        num.clusters = 10000,
                                                        model.recipe = model.recipe,
                                                        ex.classifiers = ex.classifiers,
                                                        ig.classifiers = ig.classifiers,
                                                        metrics = metrics,
                                                        saveAllModels = saveAllModels)),
                           "[D2MCS][WARNING] Number of clusters is higher than number of existing clusters. Using all clusters",
                           fixed = TRUE)

  testthat::expect_message(suppressWarnings(d2mcs$train(train.set = train.set,
                                                        train.function = train.function,
                                                        num.clusters = num.clusters,
                                                        model.recipe = model.recipe,
                                                        ex.classifiers = ex.classifiers,
                                                        ig.classifiers = c("ranger"),
                                                        metrics = metrics,
                                                        saveAllModels = saveAllModels)),
                           "[D2MCS][INFO] Ignoring '1' M.L models",
                           fixed = TRUE)

  testthat::expect_error(d2mcs$train(train.set = train.set,
                                     train.function = train.function,
                                     num.clusters = num.clusters,
                                     model.recipe = model.recipe,
                                     ex.classifiers = ex.classifiers,
                                     ig.classifiers = d2mcs$.__enclos_env__$private$loadAvailableModels()[["name"]],
                                     metrics = metrics,
                                     saveAllModels = saveAllModels),
                         "[D2MCS][FATAL] Not valid M.L models were selected. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(d2mcs$train(train.set = train.set,
                                     train.function = train.function,
                                     num.clusters = num.clusters,
                                     model.recipe = model.recipe,
                                     ex.classifiers = ex.classifiers,
                                     ig.classifiers = ig.classifiers,
                                     metrics = NULL,
                                     saveAllModels = saveAllModels),
                         "[D2MCS][FATAL] Invalid values of metrics",
                         fixed = TRUE)
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "D2MCS"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "D2MCS"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("D2MCS: getAvailableModels function works", {
  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "D2MCS"),
                            winslash = "/",
                            mustWork = FALSE)
  num.core <- 1
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  d2mcs <- D2MCS$new(dir.path = dir.path,
                     num.core = num.core,
                     socket.type = socket.type,
                     outfile = outfile,
                     serialize = serialize)

  testthat::expect_is(d2mcs$getAvailableModels(),
                      "data.frame")
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "D2MCS"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "D2MCS"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("D2MCS: classify function works", {
  testthat::skip_if_not_installed("ranger")
  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "D2MCS-classify"),
                            winslash = "/",
                            mustWork = FALSE)
  num.core <- 1
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  d2mcs <- D2MCS$new(dir.path = dir.path,
                     num.core = num.core,
                     socket.type = socket.type,
                     outfile = outfile,
                     serialize = serialize)

  train.output <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))
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

  subset <- data$createSubset(num.folds = c(2, 3),
                              class.index = "Class",
                              positive.class = "1")
  voting.types <- c(SingleVoting$new(voting.schemes = c(ClassWeightedVoting$new(cutoff = 0.7),
                                                        ProbAverageWeightedVoting$new(cutoff = 0.7),
                                                        ProbAverageVoting$new(cutoff = 0.7),
                                                        ClassMajorityVoting$new(cutoff = 0.7)),
                                     metrics = c("MCC")),
                    CombinedVoting$new(voting.schemes = ClassWeightedVoting$new(),
                                       combined.metrics = MinimizeFP$new(),
                                       methodology = ProbBasedMethodology$new(),
                                       metrics = c("MCC", "PPV")))
  positive.class <- 0

  testthat::expect_is(suppressWarnings(d2mcs$classify(train.output = train.output,
                                                      subset = subset,
                                                      voting.types = voting.types,
                                                      positive.class = positive.class)),
                      "ClassificationOutput")

  testthat::expect_message(suppressWarnings(d2mcs$classify(train.output = train.output,
                                                           subset = subset,
                                                           voting.types = voting.types,
                                                           positive.class = NULL)),
                           "[D2MCS][WARNING] Positive class not set. Asuming positive class value used during training stage '1'",
                           fixed = TRUE)

  testthat::expect_message(suppressWarnings(d2mcs$classify(train.output = train.output,
                                                           subset = subset,
                                                           voting.types = voting.types,
                                                           positive.class = 10)),
                           "[D2MCS][WARNING] Positive class value is invalid. Must be [1, 0]. Assuming positive class used during training stage (1)",
                           fixed = TRUE)
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "D2MCS-classify"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "D2MCS-classify"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("D2MCS: classify function checks type parameter", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "D2MCS-classify"),
                            winslash = "/",
                            mustWork = FALSE)
  num.core <- 1
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  d2mcs <- D2MCS$new(dir.path = dir.path,
                     num.core = num.core,
                     socket.type = socket.type,
                     outfile = outfile,
                     serialize = serialize)

  train.output <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))
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

  subset <- data$createSubset(num.folds = c(2, 3),
                              class.index = "Class",
                              positive.class = "1")
  voting.types <- c(SingleVoting$new(voting.schemes = c(ClassWeightedVoting$new(cutoff = 0.7),
                                                        ProbAverageWeightedVoting$new(cutoff = 0.7),
                                                        ProbAverageVoting$new(cutoff = 0.7),
                                                        ClassMajorityVoting$new(cutoff = 0.7)),
                                     metrics = c("MCC")),
                    CombinedVoting$new(voting.schemes = ClassWeightedVoting$new(),
                                       combined.metrics = MinimizeFP$new(),
                                       methodology = ProbBasedMethodology$new(),
                                       metrics = c("MCC", "PPV")))
  positive.class <- "0"

  testthat::expect_error(d2mcs$classify(train.output = NULL,
                                        subset = subset,
                                        voting.types = voting.types,
                                        positive.class = positive.class),
                         "[D2MCS][FATAL] Train output parameter must be defined as 'TrainOutput' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(d2mcs$classify(train.output = train.output,
                                        subset = NULL,
                                        voting.types = voting.types,
                                        positive.class = positive.class),
                         "[D2MCS][FATAL] Subset parameter must be defined as 'Subset' or 'HDSubset' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(d2mcs$classify(train.output = train.output,
                                        subset = subset,
                                        voting.types = NULL,
                                        positive.class = positive.class),
                         "[D2MCS][FATAL] Voting types parameter is not defined. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(d2mcs$classify(train.output = train.output,
                                        subset = subset,
                                        voting.types = c("wrong"),
                                        positive.class = positive.class),
                         "[D2MCS][FATAL] Voting Schemes parameter must be defined as 'SingleVoting' or 'CombinedVoting' types. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "D2MCS-classify"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "D2MCS-classify"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})
