testthat::setup({
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels",
                                                 "initializeTest"),
                                winslash = "/",
                                mustWork = FALSE))) {

    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "testExecutedModels",
                                                     "initializeTest"),
                                    winslash = "/",
                                    mustWork = FALSE),
               recursive = TRUE)

    file.copy(from = file.path("resourceFiles",
                               "lda.rds"),
              to = normalizePath(path = file.path(tempdir(),
                                                  "testExecutedModels",
                                                  "initializeTest",
                                                  "lda.rds"),
                                 winslash = "/",
                                 mustWork = FALSE))

    writeLines(c('"model","performance","exec.time"',
                 '"lda",0.7583333,1.939',
                 '"nb",0.5,2'),
               normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels",
                                              "initializeTest",
                                              "executed"),
                             winslash = "/",
                             mustWork = FALSE))
  }

  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels",
                                                 "initializeWithoutModel"),
                                winslash = "/",
                                mustWork = FALSE))) {

    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "testExecutedModels",
                                                     "initializeWithoutModel"),
                                    winslash = "/",
                                    mustWork = FALSE),
               recursive = TRUE)

    writeLines(c('"model","performance","exec.time"',
                 '"lda",0.7583333,1.939'),
               normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels",
                                              "initializeWithoutModel",
                                              "executed"),
                             winslash = "/",
                             mustWork = FALSE))
  }
})

testthat::test_that("ExecutedModels: initialize function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "dirpathEmpty"),
                            winslash = "/",
                            mustWork = FALSE)

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_true(file.exists(normalizePath(path = file.path(tempdir(),
                                                                   "testExecutedModels",
                                                                   "dirpathEmpty",
                                                                   "executed"),
                                                  winslash = "/",
                                                  mustWork = FALSE)))

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "initializeTest"),
                            winslash = "/",
                            mustWork = FALSE)

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_true(executedModels$exist(model.name = "lda"))

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "initializeWithoutModel"),
                            winslash = "/",
                            mustWork = FALSE)

  testthat::expect_message(ExecutedModels$new(dir.path = dir.path),
                           "[ExecutedModels][WARNING] Best model cannot be loaded.",
                           fixed = TRUE)
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "testExecutedModels"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels",
                                                 "initializeTest"),
                                winslash = "/",
                                mustWork = FALSE))) {

    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "testExecutedModels",
                                                     "initializeTest"),
                                    winslash = "/",
                                    mustWork = FALSE),
               recursive = TRUE)

    file.copy(from = file.path("resourceFiles",
                               "lda.rds"),
              to = normalizePath(path = file.path(tempdir(),
                                                  "testExecutedModels",
                                                  "initializeTest",
                                                  "lda.rds"),
                                 winslash = "/",
                                 mustWork = FALSE))

    writeLines(c('"model","performance","exec.time"',
                 '"lda",0.7583333,1.939',
                 '"nb",0.5,2'),
               normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels",
                                              "initializeTest",
                                              "executed"),
                             winslash = "/",
                             mustWork = FALSE))
  }
})

testthat::test_that("ExecutedModels: getNames function works", {
  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "dirpathEmpty"),
                            winslash = "/",
                            mustWork = FALSE)

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_null(executedModels$getNames())

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "initializeTest"),
                            winslash = "/",
                            mustWork = FALSE)

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_equal(executedModels$getNames(), c("lda", "nb"))
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "testExecutedModels"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels",
                                                 "initializeTest"),
                                winslash = "/",
                                mustWork = FALSE))) {

    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "testExecutedModels",
                                                     "initializeTest"),
                                winslash = "/",
                                mustWork = FALSE),
               recursive = TRUE)

    file.copy(from = file.path("resourceFiles",
                               "lda.rds"),
              to = normalizePath(path = file.path(tempdir(),
                                                  "testExecutedModels",
                                                  "initializeTest",
                                                  "lda.rds"),
                                 winslash = "/",
                                 mustWork = FALSE))

    writeLines(c('"model","performance","exec.time"',
                 '"lda",0.7583333,1.939',
                 '"nb",0.5,2'),
               normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels",
                                              "initializeTest",
                                              "executed"),
                             winslash = "/",
                             mustWork = FALSE))
  }
})

testthat::test_that("ExecutedModels: getBest function works", {
  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "initializeTest"),
                            winslash = "/",
                            mustWork = FALSE)

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_is(executedModels$getBest(), "list")
  testthat::expect_length(executedModels$getBest(), 4)

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "dirpathEmpty"),
                            winslash = "/",
                            mustWork = FALSE)

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_message(executedModels$getBest(),
                           "[ExecutedModels][WARNING] Best model not found.",
                           fixed = TRUE)
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels",
                                                 "addTest-keepBest-FALSE"),
                                winslash = "/",
                                mustWork = FALSE))) {

    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "testExecutedModels",
                                                 "addTest-keepBest-FALSE"),
                                winslash = "/",
                                mustWork = FALSE),
              recursive = TRUE)

    file.copy(from = file.path("resourceFiles",
                               "lda.rds"),
              to = normalizePath(path = file.path(tempdir(),
                                                  "testExecutedModels",
                                                  "addTest-keepBest-FALSE",
                                                  "lda.rds"),
                                 winslash = "/",
                                 mustWork = FALSE))

    writeLines(c('"model","performance","exec.time"',
                 '"lda",0.7583333,1.939',
                 '"nb",0.5,2'),
               normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels",
                                              "addTest-keepBest-FALSE",
                                              "executed"),
                             winslash = "/",
                             mustWork = FALSE))
  }
})

testthat::test_that("ExecutedModels: add function works (keep.best = FALSE)", {
  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "addTest-keepBest-FALSE"),
                            winslash = "/",
                            mustWork = FALSE)

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  model <- readRDS(file.path("resourceFiles",
                             "testExecutedModels",
                             "modelClassTrained.rds"))

  model$.__enclos_env__$private$dir.path <- dir.path
  model$.__enclos_env__$private$RDS.path <- file.path(dir.path,
                                                      "newModel.rds")

  keep.best <- FALSE

  executedModels$add(model = model,
                     keep.best = keep.best)

  testthat::expect_equal(executedModels$getNames(), c("lda", "nb", "newModel"))
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "testExecutedModels"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels",
                                                 "addTest-keepBest-TRUE"),
                                winslash = "/",
                                mustWork = FALSE))) {

    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "testExecutedModels",
                                                 "addTest-keepBest-TRUE"),
                                    winslash = "/",
                                    mustWork = FALSE),
               recursive = TRUE)

    file.copy(from = file.path("resourceFiles",
                               "lda.rds"),
              to = normalizePath(path = file.path(tempdir(),
                                                  "testExecutedModels",
                                                  "addTest-keepBest-TRUE",
                                                  "lda.rds"),
                                 winslash = "/",
                                 mustWork = FALSE))

    writeLines(c('"model","performance","exec.time"',
                 '"lda",0.7583333,1.939',
                 '"nb",0.5,2'),
               normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels",
                                              "addTest-keepBest-TRUE",
                                              "executed"),
                             winslash = "/",
                             mustWork = FALSE))
  }
})

testthat::test_that("ExecutedModels: add function works (keep.best = TRUE)", {
  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "addTest-keepBest-TRUE"),
                            winslash = "/",
                            mustWork = FALSE)

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  model <- readRDS(file.path("resourceFiles",
                             "testExecutedModels",
                             "modelClassTrained.rds"))

  model$.__enclos_env__$private$dir.path <- dir.path
  model$.__enclos_env__$private$RDS.path <- file.path(dir.path,
                                                      "newModel.rds")

  testthat::expect_true(file.exists(normalizePath(path = file.path(tempdir(),
                                                                   "testExecutedModels",
                                                                   "addTest-keepBest-TRUE",
                                                                   "lda.rds"),
                                                  winslash = "/",
                                                  mustWork = FALSE)))

  testthat::expect_false(file.exists(normalizePath(path = file.path(tempdir(),
                                                                    "testExecutedModels",
                                                                    "addTest-keepBest-TRUE",
                                                                    "newModel.rds"),
                                                   winslash = "/",
                                                   mustWork = FALSE)))

  keep.best <- TRUE

  testthat::expect_message(executedModels$add(model = model,
                                              keep.best = keep.best),
                           "[ExecutedModels][INFO] Best model found. Replacing 'lda' with 'newModel'",
                           fixed = TRUE)

  testthat::expect_false(file.exists(normalizePath(path = file.path(tempdir(),
                                                                    "testExecutedModels",
                                                                    "addTest-keepBest-TRUE",
                                                                    "lda.rds"),
                                                   winslash = "/",
                                                   mustWork = FALSE)))

  testthat::expect_true(file.exists(normalizePath(path = file.path(tempdir(),
                                                                   "testExecutedModels",
                                                                   "addTest-keepBest-TRUE",
                                                                   "newModel.rds"),
                                                  winslash = "/",
                                                  mustWork = FALSE)))

  testthat::expect_equal(executedModels$getNames(), c("lda", "nb", "newModel"))
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "testExecutedModels"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels",
                                                 "initializeTest"),
                                winslash = "/",
                                mustWork = FALSE))) {

    dir.create(normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels",
                                              "initializeTest"),
                             winslash = "/",
                             mustWork = FALSE),
               recursive = TRUE)

    file.copy(from = file.path("resourceFiles",
                               "lda.rds"),
              to = normalizePath(path = file.path(tempdir(),
                                                  "testExecutedModels",
                                                  "initializeTest",
                                                  "lda.rds"),
                                 winslash = "/",
                                 mustWork = FALSE))

    writeLines(c('"model","performance","exec.time"',
                 '"lda",0.7583333,1.939',
                 '"nb",0.5,2'),
               normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels",
                                              "initializeTest",
                                              "executed"),
                             winslash = "/",
                             mustWork = FALSE))
  }
})

testthat::test_that("ExecutedModels: add function checks parameter", {
  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "initializeTest"),
                            winslash = "/",
                            mustWork = FALSE)

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  model <- NULL
  keep.best <- TRUE

  testthat::expect_message(executedModels$add(model = model,
                                              keep.best = keep.best),
                           "[ExecutedModels][ERROR] Model parameter must be defined as 'Model' type. Model not inserted. Task not performed",
                           fixed = TRUE)
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "testExecutedModels"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({

  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels",
                                                 "initializeTest"),
                                winslash = "/",
                                mustWork = FALSE))) {

    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "testExecutedModels",
                                                     "initializeTest"),
                                    winslash = "/",
                                    mustWork = FALSE),
               recursive = TRUE)

    file.copy(from = file.path("resourceFiles",
                               "lda.rds"),
              to = normalizePath(path = file.path(tempdir(),
                                                  "testExecutedModels",
                                                  "initializeTest",
                                                  "lda.rds"),
                                 winslash = "/",
                                 mustWork = FALSE))

    writeLines(c('"model","performance","exec.time"',
                 '"lda",0.7583333,1.939',
                 '"nb",0.5,2'),
               normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels",
                                              "initializeTest",
                                              "executed"),
                             winslash = "/",
                             mustWork = FALSE))
  }
})

testthat::test_that("ExecutedModels: exist function works", {
  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "initializeTest"),
                            winslash = "/",
                            mustWork = FALSE)

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_true(executedModels$exist(model.name = "lda"))
  testthat::expect_false(executedModels$exist(model.name = list()))
  testthat::expect_false(executedModels$exist(model.name = "wrong"))
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "testExecutedModels"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({

  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels",
                                                 "initializeTest"),
                                winslash = "/",
                                mustWork = FALSE))) {

    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "testExecutedModels",
                                                     "initializeTest"),
                                winslash = "/",
                                mustWork = FALSE),
               recursive = TRUE)

    file.copy(from = file.path("resourceFiles",
                               "lda.rds"),
              to = normalizePath(path = file.path(tempdir(),
                                                  "testExecutedModels",
                                                  "initializeTest",
                                                  "lda.rds"),
                                 winslash = "/",
                                 mustWork = FALSE))

    writeLines(c('"model","performance","exec.time"',
                 '"lda",0.7583333,1.939',
                 '"nb",0.5,2'),
               normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels",
                                              "initializeTest",
                                              "executed"),
                             winslash = "/",
                             mustWork = FALSE))
  }
})

testthat::test_that("ExecutedModels: size function works", {
  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "initializeTest"),
                            winslash = "/",
                            mustWork = FALSE)

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_equal(executedModels$size(), 2)
})

testthat::teardown({
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "testExecutedModels"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("ExecutedModels: save function (file is empty)", {
  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "dirpathEmpty"),
                            winslash = "/",
                            mustWork = FALSE)

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_message(executedModels$save(),
                           "[ExecutedModels][ERROR] File is empty. Task not performed",
                           fixed = TRUE)
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels",
                                                 "deleteTest"),
                                winslash = "/",
                                mustWork = FALSE))) {

    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "testExecutedModels",
                                                     "deleteTest"),
                                    winslash = "/",
                                    mustWork = FALSE),
               recursive = TRUE)

    file.copy(from = file.path("resourceFiles",
                               "lda.rds"),
              to = normalizePath(path = file.path(tempdir(),
                                                  "testExecutedModels",
                                                  "deleteTest",
                                                  "lda.rds"),
                                 winslash = "/",
                                 mustWork = FALSE))

    writeLines(c('"model","performance","exec.time"',
                 '"lda",0.7583333,1.939',
                 '"nb",0.5,2'),
               normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels",
                                              "deleteTest",
                                              "executed"),
                             winslash = "/",
                             mustWork = FALSE))
  }
})

testthat::test_that("ExecutedModels: delete function works", {
  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "deleteTest"),
                            winslash = "/",
                            mustWork = FALSE)

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  executedModels$delete(model.name = "lda")

  testthat::expect_false(file.exists(normalizePath(path = file.path(tempdir(),
                                                                    "testExecutedModels",
                                                                    "deleteTest",
                                                                    "lda.rds"),
                                                   winslash = "/",
                                                   mustWork = FALSE)))
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels",
                                                 "deleteTestError"),
                                winslash = "/",
                                mustWork = FALSE))) {

    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "testExecutedModels",
                                                     "deleteTestError"),
                                    winslash = "/",
                                    mustWork = FALSE),
               recursive = TRUE)

    writeLines(c('"model","performance","exec.time"',
                 '"lda",0.7583333,1.939',
                 '"nb",0.5,2'),
               normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels",
                                              "deleteTestError",
                                              "executed"),
                             winslash = "/",
                             mustWork = FALSE))
  }
})

testthat::test_that("ExecutedModels: delete function checks parameter", {
  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testExecutedModels",
                                             "deleteTestError"),
                            winslash = "/",
                            mustWork = FALSE)

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_message(executedModels$delete(model.name = "wrong"),
                           "[ExecutedModels][ERROR] Cannot delete model. Model 'wrong' has not been executed. Task not performed",
                           fixed = TRUE)

  testthat::expect_message(executedModels$delete(model.name = "nb"),
                           "[ExecutedModels][ERROR] Cannot delete model. Path for model 'nb' not found. Task not performed",
                           fixed = TRUE)
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testExecutedModels"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testExecutedModels"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})
