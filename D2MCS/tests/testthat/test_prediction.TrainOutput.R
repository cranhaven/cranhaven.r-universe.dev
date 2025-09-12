testthat::test_that("TrainOutput: initialize function checks parameter type", {

  testthat::expect_error(TrainOutput$new(models = NULL,
                                         class.values = NULL,
                                         positive.class = NULL),
                         "[TrainOutput][FATAL] Models parameter must be defined as 'list' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(TrainOutput$new(models = list("example"),
                                         class.values = NULL,
                                         positive.class = NULL),
                         "[TrainOutput][FATAL] Class values parameter must be defined as 'factor' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(TrainOutput$new(models = list("example"),
                                         class.values = factor(c(0, 1, 1, 0)),
                                         positive.class = NULL),
                         "[TrainOutput][FATAL] Positive class parameter not found. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("TrainOutput: getModels function works", {
  trainOutputObject <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))

  trainOutput <- TrainOutput$new(models = trainOutputObject$.__enclos_env__$private$models,
                                 class.values = trainOutputObject$.__enclos_env__$private$class.values,
                                 positive.class = trainOutputObject$.__enclos_env__$private$positive.class)

  testthat::expect_equal(trainOutput$getModels("MCC"),
                         trainOutputObject$.__enclos_env__$private$models[["MCC"]])
})

testthat::test_that("TrainOutput: getModels function checks parameter type", {
  trainOutputObject <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))

  trainOutput <- TrainOutput$new(models = trainOutputObject$.__enclos_env__$private$models,
                                 class.values = trainOutputObject$.__enclos_env__$private$class.values,
                                 positive.class = trainOutputObject$.__enclos_env__$private$positive.class)

  testthat::expect_error(trainOutput$getModels(metric = NULL),
                         "[TrainOutput][FATAL] Metric not defined or invalid. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("TrainOutput: getModels function works", {
  trainOutputObject <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))

  trainOutput <- TrainOutput$new(models = trainOutputObject$.__enclos_env__$private$models,
                                 class.values = trainOutputObject$.__enclos_env__$private$class.values,
                                 positive.class = trainOutputObject$.__enclos_env__$private$positive.class)

  testthat::expect_is(trainOutput$getPerformance(metrics = "MCC"),
                      "list")

  testthat::expect_message(trainOutput$getPerformance(),
                           "[TrainOutput][INFO] Metrics not defined or invalid. Asuming all available metrics 'MCC, PPV'",
                           fixed = TRUE)
})

testthat::test_that("TrainOutput: savePerformance function works", {
  trainOutputObject <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))

  trainOutput <- TrainOutput$new(models = trainOutputObject$.__enclos_env__$private$models,
                                 class.values = trainOutputObject$.__enclos_env__$private$class.values,
                                 positive.class = trainOutputObject$.__enclos_env__$private$positive.class)

  dir.path <- file.path("resourceFiles", "TrainOutput")
  trainOutput$savePerformance(dir.path = dir.path)
  testthat::expect_true(file.exists(file.path(dir.path, "Performance_Train_Measures.csv")))

  testthat::expect_message(trainOutput$savePerformance(dir.path = dir.path),
                           "[TrainOutput][INFO] Folder already exists",
                           fixed = TRUE)
})

testthat::teardown({
  if (dir.exists(file.path("resourceFiles", "TrainOutput"))) {
    unlink(file.path("resourceFiles", "TrainOutput"), recursive = TRUE, force = TRUE)
  }
})

testthat::test_that("TrainOutput: savePerformance function checks parameter type", {
  trainOutputObject <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))

  trainOutput <- TrainOutput$new(models = trainOutputObject$.__enclos_env__$private$models,
                                 class.values = trainOutputObject$.__enclos_env__$private$class.values,
                                 positive.class = trainOutputObject$.__enclos_env__$private$positive.class)

  testthat::expect_error(trainOutput$savePerformance(dir.path = NULL),
                         "[TrainOutput][FATAL] Save folder not set. Aborting...",
                         fixed = TRUE)
})

testthat::setup({
  if (dir.exists(file.path("resourceFiles", "TrainOutput"))) {
    unlink(file.path("resourceFiles", "TrainOutput"), recursive = TRUE, force = TRUE)
  }
})

testthat::test_that("TrainOutput: plot function works", {
  testthat::skip_if_not_installed("grDevices")
  trainOutputObject <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))

  trainOutput <- TrainOutput$new(models = trainOutputObject$.__enclos_env__$private$models,
                                 class.values = trainOutputObject$.__enclos_env__$private$class.values,
                                 positive.class = trainOutputObject$.__enclos_env__$private$positive.class)

  dir.path <- file.path("resourceFiles", "TrainOutput")
  grDevices::pdf(NULL)
  trainOutput$plot(dir.path = dir.path)
  testthat::expect_true(file.exists(file.path(dir.path, "Performance_Train_Plot_PPV.pdf")))

  testthat::expect_message(trainOutput$plot(dir.path = dir.path),
                           "[TrainOutput][INFO] Folder already exists",
                           fixed = TRUE)
})

testthat::teardown({
  if (dir.exists(file.path("resourceFiles", "TrainOutput"))) {
    unlink(file.path("resourceFiles", "TrainOutput"), recursive = TRUE, force = TRUE)
  }
})

testthat::test_that("TrainOutput: plot function checks parameter type", {
  trainOutputObject <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))

  trainOutput <- TrainOutput$new(models = trainOutputObject$.__enclos_env__$private$models,
                                 class.values = trainOutputObject$.__enclos_env__$private$class.values,
                                 positive.class = trainOutputObject$.__enclos_env__$private$positive.class)

  testthat::expect_error(trainOutput$plot(dir.path = NULL),
                         "[TrainOutput][FATAL] Save folder not set. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("TrainOutput: getMetrics function works", {
  trainOutputObject <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))

  trainOutput <- TrainOutput$new(models = trainOutputObject$.__enclos_env__$private$models,
                                 class.values = trainOutputObject$.__enclos_env__$private$class.values,
                                 positive.class = trainOutputObject$.__enclos_env__$private$positive.class)

  testthat::expect_equal(trainOutput$getMetrics(),
                         c("MCC", "PPV"))
})

testthat::test_that("TrainOutput: getClassValues function works", {
  trainOutputObject <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))

  trainOutput <- TrainOutput$new(models = trainOutputObject$.__enclos_env__$private$models,
                                 class.values = trainOutputObject$.__enclos_env__$private$class.values,
                                 positive.class = trainOutputObject$.__enclos_env__$private$positive.class)

  testthat::expect_equal(trainOutput$getClassValues(),
                         trainOutputObject$.__enclos_env__$private$class.values)
})

testthat::test_that("TrainOutput: getPositiveClass function works", {
  trainOutputObject <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))

  trainOutput <- TrainOutput$new(models = trainOutputObject$.__enclos_env__$private$models,
                                 class.values = trainOutputObject$.__enclos_env__$private$class.values,
                                 positive.class = trainOutputObject$.__enclos_env__$private$positive.class)

  testthat::expect_equal(trainOutput$getPositiveClass(),
                         1)
})

testthat::test_that("TrainOutput: getSize function works", {
  trainOutputObject <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))

  trainOutput <- TrainOutput$new(models = trainOutputObject$.__enclos_env__$private$models,
                                 class.values = trainOutputObject$.__enclos_env__$private$class.values,
                                 positive.class = trainOutputObject$.__enclos_env__$private$positive.class)

  testthat::expect_equal(trainOutput$getSize(),
                         2)
})
