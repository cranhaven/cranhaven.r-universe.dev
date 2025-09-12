testthat::setup({
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel",
                                                 "initializeTest"),
                                winslash = "/",
                                mustWork = FALSE))) {

    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "testModel",
                                                     "initializeTest",
                                                     "model"),
                                winslash = "/",
                                mustWork = FALSE),
               recursive = TRUE)

    file.copy(from = file.path("resourceFiles",
                               "lda.rds"),
              to = normalizePath(path = file.path(tempdir(),
                                                  "testModel",
                                                  "initializeTest",
                                                  "model",
                                                  "lda.rds"),
                                 winslash = "/",
                                 mustWork = FALSE))
    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "testModel",
                                                     "initializeTest",
                                                     "wrongModel"),
                                winslash = "/",
                                mustWork = FALSE),
               recursive = TRUE)
    saveRDS("", normalizePath(path = file.path(tempdir(),
                                               "testModel",
                                               "initializeTest",
                                               "wrongModel",
                                               "lda.rds"),
                              winslash = "/",
                              mustWork = FALSE))
  }
})

testthat::test_that("Model: initialize function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "dirpath"),
                                winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- testthat::expect_message(Model$new(dir.path = dir.path,
                                                   model = model),
                                         "[Model][INFO] Save directory not exist. Creating...",
                                         fixed = TRUE)
  testthat::expect_is(modelClass,
                      "Model")
  testthat::expect_true(file.exists(normalizePath(path = file.path(tempdir(),
                                                                   "testModel",
                                                                   "dirpath"),
                                                  winslash = "/",
                                                  mustWork = FALSE)))

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "initializeTest",
                                             "model"),
                            winslash = "/",
                            mustWork = FALSE)

  testthat::expect_message(Model$new(dir.path = dir.path, model = model),
                           "[Model][INFO] Model 'lda' already exists. Loading...",
                           fixed = TRUE)

  testthat::expect_message(Model$new(dir.path = dir.path, model = model),
                           "[Model][INFO] 'lda', Linear Discriminant Analysis', Discriminant Analysis' has been succesfully loaded!",
                           fixed = TRUE)

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "initializeTest",
                                             "wrongModel"),
                            winslash = "/",
                            mustWork = FALSE)

  testthat::expect_message(Model$new(dir.path = dir.path, model = model),
                           "[Model][ERROR] Unable to load trained model. Task not performed",
                           fixed = TRUE)
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: initialize function checks parameter", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "dirpath"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  testthat::expect_error(Model$new(dir.path = dir.path,
                                   model = NULL),
                         "[Model][FATAL] Model was not defined. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: isTrained function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "dirpath"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_false(modelClass$isTrained())
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: getDir function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_equal(modelClass$getDir(), dir.path)
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: getName function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "dirpath"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_equal(modelClass$getName(), model$name)
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: getFamily function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "dirpath"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_equal(modelClass$getFamily(), model$family)
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: getDescription function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "dirpath"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_equal(modelClass$getDescription(), model$description)
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: train function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "dirpath"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  train.set <- data.frame(Gender = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1),
                          Hemochro = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          HIV = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          Hallmark = c(1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                          Grams_day = c(50, 100, 100, 60, 0, 500, 200, 80, 60, 100, 100, 100, 100, 100, 100, 100, 0, 100, 80, 100, 100, 100, 100, 0, 0, 0, 75, 180, 75, 0, 0, 0, 100, 100, 250, 75, 200, 30, 4, 99, 87, 35, 90, 100, 12, 24, 100, 107, 86, 124),
                          Ascites = c(2, 2, 2, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 3, 1, 3, 3, 1, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 3, 1, 1, 2, 1, 1, 3, 2, 1, 1),
                          INR = c(0.96, 1.58, 3.14, 1.53, 1.2, 1.44, 1.29, 1.06, 1.3, 1.32, 1.24, 1.09, 1.18, 1.2, 1.35, 1.24, 1.11, 1.92, 1.34, 2.08, 1.63, 1.23, 1.09, 1.04, 1.93, 1.17, 1.48, 1.46, 1.56, 1.13, 1.24, 1.19, 1.26, 1, 1.39, 1.63, 2.14, 1.27, 1.39, 1.26, 1.46, 1.36, 1.3, 1.62, 1.12, 1.2, 1.57, 1.35, 1.55, 1.33),
                          MCV = c(79.8, 91.5, 107.5, 90.1, 93.8, 103.4, 101, 90.7, 89.5, 96.1, 97.7, 105, 93.8, 91.4, 95.1, 83, 92.4, 119, 81.8, 91.1, 99, 90, 97.6, 98, 93.3, 93.8, 101.5, 103.8, 85.6, 95.1, 96.4, 97.3, 106.3, 94, 93.9, 101.6, 117.3, 95.6, 88, 103.8, 109.5, 88.9, 87.3, 101.8, 92.3, 86.5, 112.2, 95.2, 96.3, 86),
                          Platelets = c(472, 85, 70, 207000, 91000, 101000, 109000, 187, 108, 268000, 170000, 230000, 167000, 275000, 216, 1.71, 270000, 80000, 561, 91000, 75000, 38000, 169000, 77000, 406000, 144000, 120, 53000, 132000, 254000, 280000, 133000, 122, 157000, 88000, 172000, 118000, 272105.73, 174381.93, 175896.57, 68274.47, 114.42, 130783.68, 85246.57, 270585.81, 273354.81, 68596.59, 332033.67, 195.76, 101884.41),
                          Albumin = c(3.3, 3.4, 1.9, 4.4, 4.5, 3.4, 3.6, 4.5, 3, 3.4, 4.2, 4.2, 4.9, 3.11, 2.7, 3.9, 4, 3.1, 2.6, 2.4, 3.5, 2.2, 4.2, 3.5, 2.9, 3.8, 2.2, 3.2, 2.6, 3.68, 4.1, 4.5, 3, 3.88, 2.7, 3.44, 4.8, 3.73, 3.21, 2.43, 2.57, 3.47, 3.79, 3.62, 3.9, 2.89, 2.51, 3.26, 2.93, 3.31),
                          AST = c(68, 122, 59, 36, 96, 87, 35, 47, 85, 29, 85, 26, 29, 94, 523, 28, 73, 357, 43, 145, 85, 51, 31, 192, 266, 74, 71, 87, 219, 38, 52, 63, 401, 51, 73, 95, 60, 124, 20, 114, 86, 80, 60, 184, 75, 67, 106, 56, 68, 48),
                          ALP = c(109, 396, 63, 74, 70, 147, 141, 97, 293, 135, 227, 92, 68, 350, 397, 120, 103, 174, 88, 190, 165, 474, 91, 262, 670, 312, 97, 239, 363, 127, 123, 89, 93, 141, 44, 139, 170, 251, 913, 162, 97, 335, 181, 176, 132, 131, 85, 231, 304, 197),
                          Creatinine = c(2.1, 0.9, 0.59, 0.73, 0.88, 0.9, 0.68, 0.75, 0.67, 0.9, 1.72, 0.8, 0.72, 1.7, 0.82, 0.58, 1.24, 0.99, 0.9, 0.9, 0.7, 2.69, 1.9, 1.2, 4.82, 1.01, 2.82, 0.72, 0.55, 1.11, 0.82, 0.78, 1, 1.1, 0.96, 0.9, 0.74, 1.1, 1.3, 0.68, 0.82, 0.7, 1.17, 0.82, 1.29, 0.61, 0.8, 0.78, 1.07, 1.08),
                          Dir_Bil = c(0.1, 1.4, 1.2, 0.8, 0.2, 1.6, 0.7, 0.2, 0.4, 0.3, 0.3, 0.3, 0.3, 0.8, 5.5, 0.85, 0.2, 4.6, 0.5, 9.6, 1.7, 1, 0.85, 19.5, 29.3, 0.5, 0.3, 1, 1.5, 0.2, 0.5, 0.8, 0.4, 0.33, 1.2, 2.9, 1.8, 1.37, 0.15, 0.56, 3.1, 0.62, 0.75, 2.5, 0.27, 0.25, 1.18, 1.04, 1.57, 0.63),
                          Iron = c(28, 53, 85, 94, 82, 67, 152.6, 87, 94, 59, 104, 52.5, 52.5, 84, 56, 32, 45, 178, 19, 224, 200, 224, 53, 121, 106, 87, 92, 152.6, 40, 28, 131, 78, 124, 94, 37, 111, 161, 50.4, 15.5, 130.5, 50.3, 77.8, 99.1, 95.8, 49.6, 56, 56.9, 69.3, 71.2, 94.4),
                          Sat = c(6, 22, 73, 27, 24, 34, 39, 26, 27, 15, 37, 37, 37, 37, 27, 10, 21, 90, 8, 95, 87, 95, 21, 27, 67, 25, 56, 27, 12, 10, 78, 30, 51, 39, 17, 94, 96, 25, 5, 78, 19, 28, 38, 44, 23, 25, 27, 23, 29, 83),
                          Ferritin = c(16, 111, 982, 70, 80, 774, 76.9, 84, 70, 22, 635, 856, 856, 497, 742, 18, 802, 960, 141, 363, 316, 363, 278, 749, 2165, 81, 48.9, 76.9, 57, 308, 1316, 220, 642, 344, 419, 1600, 297, 828, 147, 1323, 342, 173, 620, 501, 766, 307, 366, 70, 106, 859),
                          Class = c("X1", "X0", "X1", "X1", "X1", "X0", "X1", "X1", "X0", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0"),
                          row.names = c(1, 4, 7, 11, 12, 18, 19, 20, 27, 31, 32, 37, 44, 50, 53, 56, 58, 74, 77, 78, 80, 88, 93, 100, 102, 103, 104, 118, 121, 127, 134, 137, 143, 144, 151, 154, 162, 165, 166, 171, 176, 178, 181, 182, 186, 191, 193, 199, 200, 201))

  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))

  trFunction <- TwoClass$new(method = "cv",
                             number = 10,
                             savePredictions = "final",
                             classProbs = TRUE,
                             allowParallel = TRUE,
                             verboseIter = FALSE,
                             seed = 1844523989)
  trFunction$create(summaryFunction = UseProbability$new(),
                    search.method = "random")
  metric <- "PPV"
  logs <- normalizePath(path = file.path(tempdir(),
                                         "testModel"),
                        winslash = "/",
                        mustWork = FALSE)
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_true(modelClass$isTrained())

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "[Model][INFO][lda] Model has already been trained",
                           fixed = TRUE)
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: train function checks parameter", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "dirpath"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  train.set <- data.frame(Gender = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1),
                          Hemochro = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          HIV = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          Hallmark = c(1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                          Grams_day = c(50, 100, 100, 60, 0, 500, 200, 80, 60, 100, 100, 100, 100, 100, 100, 100, 0, 100, 80, 100, 100, 100, 100, 0, 0, 0, 75, 180, 75, 0, 0, 0, 100, 100, 250, 75, 200, 30, 4, 99, 87, 35, 90, 100, 12, 24, 100, 107, 86, 124),
                          Ascites = c(2, 2, 2, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 3, 1, 3, 3, 1, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 3, 1, 1, 2, 1, 1, 3, 2, 1, 1),
                          INR = c(0.96, 1.58, 3.14, 1.53, 1.2, 1.44, 1.29, 1.06, 1.3, 1.32, 1.24, 1.09, 1.18, 1.2, 1.35, 1.24, 1.11, 1.92, 1.34, 2.08, 1.63, 1.23, 1.09, 1.04, 1.93, 1.17, 1.48, 1.46, 1.56, 1.13, 1.24, 1.19, 1.26, 1, 1.39, 1.63, 2.14, 1.27, 1.39, 1.26, 1.46, 1.36, 1.3, 1.62, 1.12, 1.2, 1.57, 1.35, 1.55, 1.33),
                          MCV = c(79.8, 91.5, 107.5, 90.1, 93.8, 103.4, 101, 90.7, 89.5, 96.1, 97.7, 105, 93.8, 91.4, 95.1, 83, 92.4, 119, 81.8, 91.1, 99, 90, 97.6, 98, 93.3, 93.8, 101.5, 103.8, 85.6, 95.1, 96.4, 97.3, 106.3, 94, 93.9, 101.6, 117.3, 95.6, 88, 103.8, 109.5, 88.9, 87.3, 101.8, 92.3, 86.5, 112.2, 95.2, 96.3, 86),
                          Platelets = c(472, 85, 70, 207000, 91000, 101000, 109000, 187, 108, 268000, 170000, 230000, 167000, 275000, 216, 1.71, 270000, 80000, 561, 91000, 75000, 38000, 169000, 77000, 406000, 144000, 120, 53000, 132000, 254000, 280000, 133000, 122, 157000, 88000, 172000, 118000, 272105.73, 174381.93, 175896.57, 68274.47, 114.42, 130783.68, 85246.57, 270585.81, 273354.81, 68596.59, 332033.67, 195.76, 101884.41),
                          Albumin = c(3.3, 3.4, 1.9, 4.4, 4.5, 3.4, 3.6, 4.5, 3, 3.4, 4.2, 4.2, 4.9, 3.11, 2.7, 3.9, 4, 3.1, 2.6, 2.4, 3.5, 2.2, 4.2, 3.5, 2.9, 3.8, 2.2, 3.2, 2.6, 3.68, 4.1, 4.5, 3, 3.88, 2.7, 3.44, 4.8, 3.73, 3.21, 2.43, 2.57, 3.47, 3.79, 3.62, 3.9, 2.89, 2.51, 3.26, 2.93, 3.31),
                          AST = c(68, 122, 59, 36, 96, 87, 35, 47, 85, 29, 85, 26, 29, 94, 523, 28, 73, 357, 43, 145, 85, 51, 31, 192, 266, 74, 71, 87, 219, 38, 52, 63, 401, 51, 73, 95, 60, 124, 20, 114, 86, 80, 60, 184, 75, 67, 106, 56, 68, 48),
                          ALP = c(109, 396, 63, 74, 70, 147, 141, 97, 293, 135, 227, 92, 68, 350, 397, 120, 103, 174, 88, 190, 165, 474, 91, 262, 670, 312, 97, 239, 363, 127, 123, 89, 93, 141, 44, 139, 170, 251, 913, 162, 97, 335, 181, 176, 132, 131, 85, 231, 304, 197),
                          Creatinine = c(2.1, 0.9, 0.59, 0.73, 0.88, 0.9, 0.68, 0.75, 0.67, 0.9, 1.72, 0.8, 0.72, 1.7, 0.82, 0.58, 1.24, 0.99, 0.9, 0.9, 0.7, 2.69, 1.9, 1.2, 4.82, 1.01, 2.82, 0.72, 0.55, 1.11, 0.82, 0.78, 1, 1.1, 0.96, 0.9, 0.74, 1.1, 1.3, 0.68, 0.82, 0.7, 1.17, 0.82, 1.29, 0.61, 0.8, 0.78, 1.07, 1.08),
                          Dir_Bil = c(0.1, 1.4, 1.2, 0.8, 0.2, 1.6, 0.7, 0.2, 0.4, 0.3, 0.3, 0.3, 0.3, 0.8, 5.5, 0.85, 0.2, 4.6, 0.5, 9.6, 1.7, 1, 0.85, 19.5, 29.3, 0.5, 0.3, 1, 1.5, 0.2, 0.5, 0.8, 0.4, 0.33, 1.2, 2.9, 1.8, 1.37, 0.15, 0.56, 3.1, 0.62, 0.75, 2.5, 0.27, 0.25, 1.18, 1.04, 1.57, 0.63),
                          Iron = c(28, 53, 85, 94, 82, 67, 152.6, 87, 94, 59, 104, 52.5, 52.5, 84, 56, 32, 45, 178, 19, 224, 200, 224, 53, 121, 106, 87, 92, 152.6, 40, 28, 131, 78, 124, 94, 37, 111, 161, 50.4, 15.5, 130.5, 50.3, 77.8, 99.1, 95.8, 49.6, 56, 56.9, 69.3, 71.2, 94.4),
                          Sat = c(6, 22, 73, 27, 24, 34, 39, 26, 27, 15, 37, 37, 37, 37, 27, 10, 21, 90, 8, 95, 87, 95, 21, 27, 67, 25, 56, 27, 12, 10, 78, 30, 51, 39, 17, 94, 96, 25, 5, 78, 19, 28, 38, 44, 23, 25, 27, 23, 29, 83),
                          Ferritin = c(16, 111, 982, 70, 80, 774, 76.9, 84, 70, 22, 635, 856, 856, 497, 742, 18, 802, 960, 141, 363, 316, 363, 278, 749, 2165, 81, 48.9, 76.9, 57, 308, 1316, 220, 642, 344, 419, 1600, 297, 828, 147, 1323, 342, 173, 620, 501, 766, 307, 366, 70, 106, 859),
                          Class = c("X1", "X0", "X1", "X1", "X1", "X0", "X1", "X1", "X0", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0"),
                          row.names = c(1, 4, 7, 11, 12, 18, 19, 20, 27, 31, 32, 37, 44, 50, 53, 56, 58, 74, 77, 78, 80, 88, 93, 100, 102, 103, 104, 118, 121, 127, 134, 137, 143, 144, 151, 154, 162, 165, 166, 171, 176, 178, 181, 182, 186, 191, 193, 199, 200, 201))

  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- TwoClass$new(method = "cv",
                             number = 10,
                             savePredictions = "final",
                             classProbs = TRUE,
                             allowParallel = TRUE,
                             verboseIter = FALSE,
                             seed = 1844523989)
  trFunction$create(summaryFunction = UseProbability$new(),
                    search.method = "random")
  metric <- "PPV"
  logs <- normalizePath(path = file.path(tempdir(),
                                         "testModel"),
                        winslash = "/",
                        mustWork = FALSE)
  file.create(file.path(logs, "error.log"))

  testthat::expect_error(modelClass$train(train.set = NULL,
                                          fitting = fitting,
                                          trFunction = trFunction,
                                          metric = metric,
                                          logs = logs),
                         "[Model][FATAL][lda] Cannot perform trainning stage. Train set must be defined as 'data.frame' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(modelClass$train(train.set = data.frame(),
                                          fitting = fitting,
                                          trFunction = trFunction,
                                          metric = metric,
                                          logs = logs),
                         "[Model][FATAL][lda] Cannot perform trainning stage. Train set is empty. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(modelClass$train(train.set = train.set,
                                          fitting = fitting,
                                          trFunction = NULL,
                                          metric = metric,
                                          logs = logs),
                         "[Model][FATAL][lda] TrainFunction must be inherits from 'TrainFunction' class. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(modelClass$train(train.set = train.set,
                                          fitting = fitting,
                                          trFunction = trFunction,
                                          metric = "WRONG",
                                          logs = logs),
                         "[Model][FATAL][lda] Metric is not defined or unavailable. Must be a [ROC, Sens, Spec, Kappa, Accuracy, TCR_9, MCC, PPV] type. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: getTrainedModel function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "dirpath"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_message(modelClass$getTrainedModel(),
                           "[Model][WARNING] Model 'lda' is not trained. Task not performed",
                           fixed = TRUE)

  train.set <- data.frame(Gender = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1),
                          Hemochro = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          HIV = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          Hallmark = c(1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                          Grams_day = c(50, 100, 100, 60, 0, 500, 200, 80, 60, 100, 100, 100, 100, 100, 100, 100, 0, 100, 80, 100, 100, 100, 100, 0, 0, 0, 75, 180, 75, 0, 0, 0, 100, 100, 250, 75, 200, 30, 4, 99, 87, 35, 90, 100, 12, 24, 100, 107, 86, 124),
                          Ascites = c(2, 2, 2, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 3, 1, 3, 3, 1, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 3, 1, 1, 2, 1, 1, 3, 2, 1, 1),
                          INR = c(0.96, 1.58, 3.14, 1.53, 1.2, 1.44, 1.29, 1.06, 1.3, 1.32, 1.24, 1.09, 1.18, 1.2, 1.35, 1.24, 1.11, 1.92, 1.34, 2.08, 1.63, 1.23, 1.09, 1.04, 1.93, 1.17, 1.48, 1.46, 1.56, 1.13, 1.24, 1.19, 1.26, 1, 1.39, 1.63, 2.14, 1.27, 1.39, 1.26, 1.46, 1.36, 1.3, 1.62, 1.12, 1.2, 1.57, 1.35, 1.55, 1.33),
                          MCV = c(79.8, 91.5, 107.5, 90.1, 93.8, 103.4, 101, 90.7, 89.5, 96.1, 97.7, 105, 93.8, 91.4, 95.1, 83, 92.4, 119, 81.8, 91.1, 99, 90, 97.6, 98, 93.3, 93.8, 101.5, 103.8, 85.6, 95.1, 96.4, 97.3, 106.3, 94, 93.9, 101.6, 117.3, 95.6, 88, 103.8, 109.5, 88.9, 87.3, 101.8, 92.3, 86.5, 112.2, 95.2, 96.3, 86),
                          Platelets = c(472, 85, 70, 207000, 91000, 101000, 109000, 187, 108, 268000, 170000, 230000, 167000, 275000, 216, 1.71, 270000, 80000, 561, 91000, 75000, 38000, 169000, 77000, 406000, 144000, 120, 53000, 132000, 254000, 280000, 133000, 122, 157000, 88000, 172000, 118000, 272105.73, 174381.93, 175896.57, 68274.47, 114.42, 130783.68, 85246.57, 270585.81, 273354.81, 68596.59, 332033.67, 195.76, 101884.41),
                          Albumin = c(3.3, 3.4, 1.9, 4.4, 4.5, 3.4, 3.6, 4.5, 3, 3.4, 4.2, 4.2, 4.9, 3.11, 2.7, 3.9, 4, 3.1, 2.6, 2.4, 3.5, 2.2, 4.2, 3.5, 2.9, 3.8, 2.2, 3.2, 2.6, 3.68, 4.1, 4.5, 3, 3.88, 2.7, 3.44, 4.8, 3.73, 3.21, 2.43, 2.57, 3.47, 3.79, 3.62, 3.9, 2.89, 2.51, 3.26, 2.93, 3.31),
                          AST = c(68, 122, 59, 36, 96, 87, 35, 47, 85, 29, 85, 26, 29, 94, 523, 28, 73, 357, 43, 145, 85, 51, 31, 192, 266, 74, 71, 87, 219, 38, 52, 63, 401, 51, 73, 95, 60, 124, 20, 114, 86, 80, 60, 184, 75, 67, 106, 56, 68, 48),
                          ALP = c(109, 396, 63, 74, 70, 147, 141, 97, 293, 135, 227, 92, 68, 350, 397, 120, 103, 174, 88, 190, 165, 474, 91, 262, 670, 312, 97, 239, 363, 127, 123, 89, 93, 141, 44, 139, 170, 251, 913, 162, 97, 335, 181, 176, 132, 131, 85, 231, 304, 197),
                          Creatinine = c(2.1, 0.9, 0.59, 0.73, 0.88, 0.9, 0.68, 0.75, 0.67, 0.9, 1.72, 0.8, 0.72, 1.7, 0.82, 0.58, 1.24, 0.99, 0.9, 0.9, 0.7, 2.69, 1.9, 1.2, 4.82, 1.01, 2.82, 0.72, 0.55, 1.11, 0.82, 0.78, 1, 1.1, 0.96, 0.9, 0.74, 1.1, 1.3, 0.68, 0.82, 0.7, 1.17, 0.82, 1.29, 0.61, 0.8, 0.78, 1.07, 1.08),
                          Dir_Bil = c(0.1, 1.4, 1.2, 0.8, 0.2, 1.6, 0.7, 0.2, 0.4, 0.3, 0.3, 0.3, 0.3, 0.8, 5.5, 0.85, 0.2, 4.6, 0.5, 9.6, 1.7, 1, 0.85, 19.5, 29.3, 0.5, 0.3, 1, 1.5, 0.2, 0.5, 0.8, 0.4, 0.33, 1.2, 2.9, 1.8, 1.37, 0.15, 0.56, 3.1, 0.62, 0.75, 2.5, 0.27, 0.25, 1.18, 1.04, 1.57, 0.63),
                          Iron = c(28, 53, 85, 94, 82, 67, 152.6, 87, 94, 59, 104, 52.5, 52.5, 84, 56, 32, 45, 178, 19, 224, 200, 224, 53, 121, 106, 87, 92, 152.6, 40, 28, 131, 78, 124, 94, 37, 111, 161, 50.4, 15.5, 130.5, 50.3, 77.8, 99.1, 95.8, 49.6, 56, 56.9, 69.3, 71.2, 94.4),
                          Sat = c(6, 22, 73, 27, 24, 34, 39, 26, 27, 15, 37, 37, 37, 37, 27, 10, 21, 90, 8, 95, 87, 95, 21, 27, 67, 25, 56, 27, 12, 10, 78, 30, 51, 39, 17, 94, 96, 25, 5, 78, 19, 28, 38, 44, 23, 25, 27, 23, 29, 83),
                          Ferritin = c(16, 111, 982, 70, 80, 774, 76.9, 84, 70, 22, 635, 856, 856, 497, 742, 18, 802, 960, 141, 363, 316, 363, 278, 749, 2165, 81, 48.9, 76.9, 57, 308, 1316, 220, 642, 344, 419, 1600, 297, 828, 147, 1323, 342, 173, 620, 501, 766, 307, 366, 70, 106, 859),
                          Class = c("X1", "X0", "X1", "X1", "X1", "X0", "X1", "X1", "X0", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0"),
                          row.names = c(1, 4, 7, 11, 12, 18, 19, 20, 27, 31, 32, 37, 44, 50, 53, 56, 58, 74, 77, 78, 80, 88, 93, 100, 102, 103, 104, 118, 121, 127, 134, 137, 143, 144, 151, 154, 162, 165, 166, 171, 176, 178, 181, 182, 186, 191, 193, 199, 200, 201))

  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- TwoClass$new(method = "cv",
                             number = 10,
                             savePredictions = "final",
                             classProbs = TRUE,
                             allowParallel = TRUE,
                             verboseIter = FALSE,
                             seed = 1844523989)
  trFunction$create(summaryFunction = UseProbability$new(),
                    search.method = "random")
  metric <- "PPV"
  logs <- normalizePath(path = file.path(tempdir(),
                                         "testModel"),
                        winslash = "/",
                        mustWork = FALSE)
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_type(modelClass$getTrainedModel(), "list")
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = file.path("resourceFiles",
                         "testModel"),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: getExecutionTime function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "dirpath"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_message(modelClass$getExecutionTime(),
                           "[Model][WARNING] Model 'lda' is not trained. Task not performed",
                           fixed = TRUE)

  train.set <- data.frame(Gender = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1),
                          Hemochro = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          HIV = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          Hallmark = c(1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                          Grams_day = c(50, 100, 100, 60, 0, 500, 200, 80, 60, 100, 100, 100, 100, 100, 100, 100, 0, 100, 80, 100, 100, 100, 100, 0, 0, 0, 75, 180, 75, 0, 0, 0, 100, 100, 250, 75, 200, 30, 4, 99, 87, 35, 90, 100, 12, 24, 100, 107, 86, 124),
                          Ascites = c(2, 2, 2, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 3, 1, 3, 3, 1, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 3, 1, 1, 2, 1, 1, 3, 2, 1, 1),
                          INR = c(0.96, 1.58, 3.14, 1.53, 1.2, 1.44, 1.29, 1.06, 1.3, 1.32, 1.24, 1.09, 1.18, 1.2, 1.35, 1.24, 1.11, 1.92, 1.34, 2.08, 1.63, 1.23, 1.09, 1.04, 1.93, 1.17, 1.48, 1.46, 1.56, 1.13, 1.24, 1.19, 1.26, 1, 1.39, 1.63, 2.14, 1.27, 1.39, 1.26, 1.46, 1.36, 1.3, 1.62, 1.12, 1.2, 1.57, 1.35, 1.55, 1.33),
                          MCV = c(79.8, 91.5, 107.5, 90.1, 93.8, 103.4, 101, 90.7, 89.5, 96.1, 97.7, 105, 93.8, 91.4, 95.1, 83, 92.4, 119, 81.8, 91.1, 99, 90, 97.6, 98, 93.3, 93.8, 101.5, 103.8, 85.6, 95.1, 96.4, 97.3, 106.3, 94, 93.9, 101.6, 117.3, 95.6, 88, 103.8, 109.5, 88.9, 87.3, 101.8, 92.3, 86.5, 112.2, 95.2, 96.3, 86),
                          Platelets = c(472, 85, 70, 207000, 91000, 101000, 109000, 187, 108, 268000, 170000, 230000, 167000, 275000, 216, 1.71, 270000, 80000, 561, 91000, 75000, 38000, 169000, 77000, 406000, 144000, 120, 53000, 132000, 254000, 280000, 133000, 122, 157000, 88000, 172000, 118000, 272105.73, 174381.93, 175896.57, 68274.47, 114.42, 130783.68, 85246.57, 270585.81, 273354.81, 68596.59, 332033.67, 195.76, 101884.41),
                          Albumin = c(3.3, 3.4, 1.9, 4.4, 4.5, 3.4, 3.6, 4.5, 3, 3.4, 4.2, 4.2, 4.9, 3.11, 2.7, 3.9, 4, 3.1, 2.6, 2.4, 3.5, 2.2, 4.2, 3.5, 2.9, 3.8, 2.2, 3.2, 2.6, 3.68, 4.1, 4.5, 3, 3.88, 2.7, 3.44, 4.8, 3.73, 3.21, 2.43, 2.57, 3.47, 3.79, 3.62, 3.9, 2.89, 2.51, 3.26, 2.93, 3.31),
                          AST = c(68, 122, 59, 36, 96, 87, 35, 47, 85, 29, 85, 26, 29, 94, 523, 28, 73, 357, 43, 145, 85, 51, 31, 192, 266, 74, 71, 87, 219, 38, 52, 63, 401, 51, 73, 95, 60, 124, 20, 114, 86, 80, 60, 184, 75, 67, 106, 56, 68, 48),
                          ALP = c(109, 396, 63, 74, 70, 147, 141, 97, 293, 135, 227, 92, 68, 350, 397, 120, 103, 174, 88, 190, 165, 474, 91, 262, 670, 312, 97, 239, 363, 127, 123, 89, 93, 141, 44, 139, 170, 251, 913, 162, 97, 335, 181, 176, 132, 131, 85, 231, 304, 197),
                          Creatinine = c(2.1, 0.9, 0.59, 0.73, 0.88, 0.9, 0.68, 0.75, 0.67, 0.9, 1.72, 0.8, 0.72, 1.7, 0.82, 0.58, 1.24, 0.99, 0.9, 0.9, 0.7, 2.69, 1.9, 1.2, 4.82, 1.01, 2.82, 0.72, 0.55, 1.11, 0.82, 0.78, 1, 1.1, 0.96, 0.9, 0.74, 1.1, 1.3, 0.68, 0.82, 0.7, 1.17, 0.82, 1.29, 0.61, 0.8, 0.78, 1.07, 1.08),
                          Dir_Bil = c(0.1, 1.4, 1.2, 0.8, 0.2, 1.6, 0.7, 0.2, 0.4, 0.3, 0.3, 0.3, 0.3, 0.8, 5.5, 0.85, 0.2, 4.6, 0.5, 9.6, 1.7, 1, 0.85, 19.5, 29.3, 0.5, 0.3, 1, 1.5, 0.2, 0.5, 0.8, 0.4, 0.33, 1.2, 2.9, 1.8, 1.37, 0.15, 0.56, 3.1, 0.62, 0.75, 2.5, 0.27, 0.25, 1.18, 1.04, 1.57, 0.63),
                          Iron = c(28, 53, 85, 94, 82, 67, 152.6, 87, 94, 59, 104, 52.5, 52.5, 84, 56, 32, 45, 178, 19, 224, 200, 224, 53, 121, 106, 87, 92, 152.6, 40, 28, 131, 78, 124, 94, 37, 111, 161, 50.4, 15.5, 130.5, 50.3, 77.8, 99.1, 95.8, 49.6, 56, 56.9, 69.3, 71.2, 94.4),
                          Sat = c(6, 22, 73, 27, 24, 34, 39, 26, 27, 15, 37, 37, 37, 37, 27, 10, 21, 90, 8, 95, 87, 95, 21, 27, 67, 25, 56, 27, 12, 10, 78, 30, 51, 39, 17, 94, 96, 25, 5, 78, 19, 28, 38, 44, 23, 25, 27, 23, 29, 83),
                          Ferritin = c(16, 111, 982, 70, 80, 774, 76.9, 84, 70, 22, 635, 856, 856, 497, 742, 18, 802, 960, 141, 363, 316, 363, 278, 749, 2165, 81, 48.9, 76.9, 57, 308, 1316, 220, 642, 344, 419, 1600, 297, 828, 147, 1323, 342, 173, 620, 501, 766, 307, 366, 70, 106, 859),
                          Class = c("X1", "X0", "X1", "X1", "X1", "X0", "X1", "X1", "X0", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0"),
                          row.names = c(1, 4, 7, 11, 12, 18, 19, 20, 27, 31, 32, 37, 44, 50, 53, 56, 58, 74, 77, 78, 80, 88, 93, 100, 102, 103, 104, 118, 121, 127, 134, 137, 143, 144, 151, 154, 162, 165, 166, 171, 176, 178, 181, 182, 186, 191, 193, 199, 200, 201))

  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- TwoClass$new(method = "cv",
                             number = 10,
                             savePredictions = "final",
                             classProbs = TRUE,
                             allowParallel = TRUE,
                             verboseIter = FALSE,
                             seed = 1844523989)
  trFunction$create(summaryFunction = UseProbability$new(),
                    search.method = "random")
  metric <- "PPV"
  logs <- normalizePath(path = file.path(tempdir(),
                                         "testModel"),
                        winslash = "/",
                        mustWork = FALSE)
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_type(modelClass$getExecutionTime(), "double")
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: getPerformance function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "dirpath"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  train.set <- data.frame(Gender = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1),
                          Hemochro = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          HIV = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          Hallmark = c(1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                          Grams_day = c(50, 100, 100, 60, 0, 500, 200, 80, 60, 100, 100, 100, 100, 100, 100, 100, 0, 100, 80, 100, 100, 100, 100, 0, 0, 0, 75, 180, 75, 0, 0, 0, 100, 100, 250, 75, 200, 30, 4, 99, 87, 35, 90, 100, 12, 24, 100, 107, 86, 124),
                          Ascites = c(2, 2, 2, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 3, 1, 3, 3, 1, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 3, 1, 1, 2, 1, 1, 3, 2, 1, 1),
                          INR = c(0.96, 1.58, 3.14, 1.53, 1.2, 1.44, 1.29, 1.06, 1.3, 1.32, 1.24, 1.09, 1.18, 1.2, 1.35, 1.24, 1.11, 1.92, 1.34, 2.08, 1.63, 1.23, 1.09, 1.04, 1.93, 1.17, 1.48, 1.46, 1.56, 1.13, 1.24, 1.19, 1.26, 1, 1.39, 1.63, 2.14, 1.27, 1.39, 1.26, 1.46, 1.36, 1.3, 1.62, 1.12, 1.2, 1.57, 1.35, 1.55, 1.33),
                          MCV = c(79.8, 91.5, 107.5, 90.1, 93.8, 103.4, 101, 90.7, 89.5, 96.1, 97.7, 105, 93.8, 91.4, 95.1, 83, 92.4, 119, 81.8, 91.1, 99, 90, 97.6, 98, 93.3, 93.8, 101.5, 103.8, 85.6, 95.1, 96.4, 97.3, 106.3, 94, 93.9, 101.6, 117.3, 95.6, 88, 103.8, 109.5, 88.9, 87.3, 101.8, 92.3, 86.5, 112.2, 95.2, 96.3, 86),
                          Platelets = c(472, 85, 70, 207000, 91000, 101000, 109000, 187, 108, 268000, 170000, 230000, 167000, 275000, 216, 1.71, 270000, 80000, 561, 91000, 75000, 38000, 169000, 77000, 406000, 144000, 120, 53000, 132000, 254000, 280000, 133000, 122, 157000, 88000, 172000, 118000, 272105.73, 174381.93, 175896.57, 68274.47, 114.42, 130783.68, 85246.57, 270585.81, 273354.81, 68596.59, 332033.67, 195.76, 101884.41),
                          Albumin = c(3.3, 3.4, 1.9, 4.4, 4.5, 3.4, 3.6, 4.5, 3, 3.4, 4.2, 4.2, 4.9, 3.11, 2.7, 3.9, 4, 3.1, 2.6, 2.4, 3.5, 2.2, 4.2, 3.5, 2.9, 3.8, 2.2, 3.2, 2.6, 3.68, 4.1, 4.5, 3, 3.88, 2.7, 3.44, 4.8, 3.73, 3.21, 2.43, 2.57, 3.47, 3.79, 3.62, 3.9, 2.89, 2.51, 3.26, 2.93, 3.31),
                          AST = c(68, 122, 59, 36, 96, 87, 35, 47, 85, 29, 85, 26, 29, 94, 523, 28, 73, 357, 43, 145, 85, 51, 31, 192, 266, 74, 71, 87, 219, 38, 52, 63, 401, 51, 73, 95, 60, 124, 20, 114, 86, 80, 60, 184, 75, 67, 106, 56, 68, 48),
                          ALP = c(109, 396, 63, 74, 70, 147, 141, 97, 293, 135, 227, 92, 68, 350, 397, 120, 103, 174, 88, 190, 165, 474, 91, 262, 670, 312, 97, 239, 363, 127, 123, 89, 93, 141, 44, 139, 170, 251, 913, 162, 97, 335, 181, 176, 132, 131, 85, 231, 304, 197),
                          Creatinine = c(2.1, 0.9, 0.59, 0.73, 0.88, 0.9, 0.68, 0.75, 0.67, 0.9, 1.72, 0.8, 0.72, 1.7, 0.82, 0.58, 1.24, 0.99, 0.9, 0.9, 0.7, 2.69, 1.9, 1.2, 4.82, 1.01, 2.82, 0.72, 0.55, 1.11, 0.82, 0.78, 1, 1.1, 0.96, 0.9, 0.74, 1.1, 1.3, 0.68, 0.82, 0.7, 1.17, 0.82, 1.29, 0.61, 0.8, 0.78, 1.07, 1.08),
                          Dir_Bil = c(0.1, 1.4, 1.2, 0.8, 0.2, 1.6, 0.7, 0.2, 0.4, 0.3, 0.3, 0.3, 0.3, 0.8, 5.5, 0.85, 0.2, 4.6, 0.5, 9.6, 1.7, 1, 0.85, 19.5, 29.3, 0.5, 0.3, 1, 1.5, 0.2, 0.5, 0.8, 0.4, 0.33, 1.2, 2.9, 1.8, 1.37, 0.15, 0.56, 3.1, 0.62, 0.75, 2.5, 0.27, 0.25, 1.18, 1.04, 1.57, 0.63),
                          Iron = c(28, 53, 85, 94, 82, 67, 152.6, 87, 94, 59, 104, 52.5, 52.5, 84, 56, 32, 45, 178, 19, 224, 200, 224, 53, 121, 106, 87, 92, 152.6, 40, 28, 131, 78, 124, 94, 37, 111, 161, 50.4, 15.5, 130.5, 50.3, 77.8, 99.1, 95.8, 49.6, 56, 56.9, 69.3, 71.2, 94.4),
                          Sat = c(6, 22, 73, 27, 24, 34, 39, 26, 27, 15, 37, 37, 37, 37, 27, 10, 21, 90, 8, 95, 87, 95, 21, 27, 67, 25, 56, 27, 12, 10, 78, 30, 51, 39, 17, 94, 96, 25, 5, 78, 19, 28, 38, 44, 23, 25, 27, 23, 29, 83),
                          Ferritin = c(16, 111, 982, 70, 80, 774, 76.9, 84, 70, 22, 635, 856, 856, 497, 742, 18, 802, 960, 141, 363, 316, 363, 278, 749, 2165, 81, 48.9, 76.9, 57, 308, 1316, 220, 642, 344, 419, 1600, 297, 828, 147, 1323, 342, 173, 620, 501, 766, 307, 366, 70, 106, 859),
                          Class = c("X1", "X0", "X1", "X1", "X1", "X0", "X1", "X1", "X0", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0"),
                          row.names = c(1, 4, 7, 11, 12, 18, 19, 20, 27, 31, 32, 37, 44, 50, 53, 56, 58, 74, 77, 78, 80, 88, 93, 100, 102, 103, 104, 118, 121, 127, 134, 137, 143, 144, 151, 154, 162, 165, 166, 171, 176, 178, 181, 182, 186, 191, 193, 199, 200, 201))

  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- TwoClass$new(method = "cv",
                             number = 10,
                             savePredictions = "final",
                             classProbs = TRUE,
                             allowParallel = TRUE,
                             verboseIter = FALSE,
                             seed = 1844523989)
  trFunction$create(summaryFunction = UseProbability$new(),
                    search.method = "random")
  metric <- "PPV"
  logs <- normalizePath(path = file.path(tempdir(),
                                         "testModel"),
                        winslash = "/",
                        mustWork = FALSE)
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_is(modelClass$getPerformance(), "numeric")
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: getPerformance function checks parameter", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "dirpath"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  train.set <- data.frame(Gender = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1),
                          Hemochro = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          HIV = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          Hallmark = c(1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                          Grams_day = c(50, 100, 100, 60, 0, 500, 200, 80, 60, 100, 100, 100, 100, 100, 100, 100, 0, 100, 80, 100, 100, 100, 100, 0, 0, 0, 75, 180, 75, 0, 0, 0, 100, 100, 250, 75, 200, 30, 4, 99, 87, 35, 90, 100, 12, 24, 100, 107, 86, 124),
                          Ascites = c(2, 2, 2, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 3, 1, 3, 3, 1, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 3, 1, 1, 2, 1, 1, 3, 2, 1, 1),
                          INR = c(0.96, 1.58, 3.14, 1.53, 1.2, 1.44, 1.29, 1.06, 1.3, 1.32, 1.24, 1.09, 1.18, 1.2, 1.35, 1.24, 1.11, 1.92, 1.34, 2.08, 1.63, 1.23, 1.09, 1.04, 1.93, 1.17, 1.48, 1.46, 1.56, 1.13, 1.24, 1.19, 1.26, 1, 1.39, 1.63, 2.14, 1.27, 1.39, 1.26, 1.46, 1.36, 1.3, 1.62, 1.12, 1.2, 1.57, 1.35, 1.55, 1.33),
                          MCV = c(79.8, 91.5, 107.5, 90.1, 93.8, 103.4, 101, 90.7, 89.5, 96.1, 97.7, 105, 93.8, 91.4, 95.1, 83, 92.4, 119, 81.8, 91.1, 99, 90, 97.6, 98, 93.3, 93.8, 101.5, 103.8, 85.6, 95.1, 96.4, 97.3, 106.3, 94, 93.9, 101.6, 117.3, 95.6, 88, 103.8, 109.5, 88.9, 87.3, 101.8, 92.3, 86.5, 112.2, 95.2, 96.3, 86),
                          Platelets = c(472, 85, 70, 207000, 91000, 101000, 109000, 187, 108, 268000, 170000, 230000, 167000, 275000, 216, 1.71, 270000, 80000, 561, 91000, 75000, 38000, 169000, 77000, 406000, 144000, 120, 53000, 132000, 254000, 280000, 133000, 122, 157000, 88000, 172000, 118000, 272105.73, 174381.93, 175896.57, 68274.47, 114.42, 130783.68, 85246.57, 270585.81, 273354.81, 68596.59, 332033.67, 195.76, 101884.41),
                          Albumin = c(3.3, 3.4, 1.9, 4.4, 4.5, 3.4, 3.6, 4.5, 3, 3.4, 4.2, 4.2, 4.9, 3.11, 2.7, 3.9, 4, 3.1, 2.6, 2.4, 3.5, 2.2, 4.2, 3.5, 2.9, 3.8, 2.2, 3.2, 2.6, 3.68, 4.1, 4.5, 3, 3.88, 2.7, 3.44, 4.8, 3.73, 3.21, 2.43, 2.57, 3.47, 3.79, 3.62, 3.9, 2.89, 2.51, 3.26, 2.93, 3.31),
                          AST = c(68, 122, 59, 36, 96, 87, 35, 47, 85, 29, 85, 26, 29, 94, 523, 28, 73, 357, 43, 145, 85, 51, 31, 192, 266, 74, 71, 87, 219, 38, 52, 63, 401, 51, 73, 95, 60, 124, 20, 114, 86, 80, 60, 184, 75, 67, 106, 56, 68, 48),
                          ALP = c(109, 396, 63, 74, 70, 147, 141, 97, 293, 135, 227, 92, 68, 350, 397, 120, 103, 174, 88, 190, 165, 474, 91, 262, 670, 312, 97, 239, 363, 127, 123, 89, 93, 141, 44, 139, 170, 251, 913, 162, 97, 335, 181, 176, 132, 131, 85, 231, 304, 197),
                          Creatinine = c(2.1, 0.9, 0.59, 0.73, 0.88, 0.9, 0.68, 0.75, 0.67, 0.9, 1.72, 0.8, 0.72, 1.7, 0.82, 0.58, 1.24, 0.99, 0.9, 0.9, 0.7, 2.69, 1.9, 1.2, 4.82, 1.01, 2.82, 0.72, 0.55, 1.11, 0.82, 0.78, 1, 1.1, 0.96, 0.9, 0.74, 1.1, 1.3, 0.68, 0.82, 0.7, 1.17, 0.82, 1.29, 0.61, 0.8, 0.78, 1.07, 1.08),
                          Dir_Bil = c(0.1, 1.4, 1.2, 0.8, 0.2, 1.6, 0.7, 0.2, 0.4, 0.3, 0.3, 0.3, 0.3, 0.8, 5.5, 0.85, 0.2, 4.6, 0.5, 9.6, 1.7, 1, 0.85, 19.5, 29.3, 0.5, 0.3, 1, 1.5, 0.2, 0.5, 0.8, 0.4, 0.33, 1.2, 2.9, 1.8, 1.37, 0.15, 0.56, 3.1, 0.62, 0.75, 2.5, 0.27, 0.25, 1.18, 1.04, 1.57, 0.63),
                          Iron = c(28, 53, 85, 94, 82, 67, 152.6, 87, 94, 59, 104, 52.5, 52.5, 84, 56, 32, 45, 178, 19, 224, 200, 224, 53, 121, 106, 87, 92, 152.6, 40, 28, 131, 78, 124, 94, 37, 111, 161, 50.4, 15.5, 130.5, 50.3, 77.8, 99.1, 95.8, 49.6, 56, 56.9, 69.3, 71.2, 94.4),
                          Sat = c(6, 22, 73, 27, 24, 34, 39, 26, 27, 15, 37, 37, 37, 37, 27, 10, 21, 90, 8, 95, 87, 95, 21, 27, 67, 25, 56, 27, 12, 10, 78, 30, 51, 39, 17, 94, 96, 25, 5, 78, 19, 28, 38, 44, 23, 25, 27, 23, 29, 83),
                          Ferritin = c(16, 111, 982, 70, 80, 774, 76.9, 84, 70, 22, 635, 856, 856, 497, 742, 18, 802, 960, 141, 363, 316, 363, 278, 749, 2165, 81, 48.9, 76.9, 57, 308, 1316, 220, 642, 344, 419, 1600, 297, 828, 147, 1323, 342, 173, 620, 501, 766, 307, 366, 70, 106, 859),
                          Class = c("X1", "X0", "X1", "X1", "X1", "X0", "X1", "X1", "X0", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0"),
                          row.names = c(1, 4, 7, 11, 12, 18, 19, 20, 27, 31, 32, 37, 44, 50, 53, 56, 58, 74, 77, 78, 80, 88, 93, 100, 102, 103, 104, 118, 121, 127, 134, 137, 143, 144, 151, 154, 162, 165, 166, 171, 176, 178, 181, 182, 186, 191, 193, 199, 200, 201))

  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- TwoClass$new(method = "cv",
                             number = 10,
                             savePredictions = "final",
                             classProbs = TRUE,
                             allowParallel = TRUE,
                             verboseIter = FALSE,
                             seed = 1844523989)
  trFunction$create(summaryFunction = UseProbability$new(),
                    search.method = "random")
  metric <- "PPV"
  logs <- normalizePath(path = file.path(tempdir(),
                                         "testModel"),
                        winslash = "/",
                        mustWork = FALSE)
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_error(modelClass$getPerformance(metric = "WRONG"),
                         "[Model][FATAL] Metric is not defined or unavailable. Must be a [ROC, Sens, Spec, Kappa, Accuracy, TCR_9, MCC, PPV] type. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: getConfiguration function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "dirpath"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_message(modelClass$getConfiguration(),
                           "[Model][WARNING] Model 'lda' is not trained. Task not performed",
                           fixed = TRUE)

  train.set <- data.frame(Gender = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1),
                          Hemochro = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          HIV = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          Hallmark = c(1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                          Grams_day = c(50, 100, 100, 60, 0, 500, 200, 80, 60, 100, 100, 100, 100, 100, 100, 100, 0, 100, 80, 100, 100, 100, 100, 0, 0, 0, 75, 180, 75, 0, 0, 0, 100, 100, 250, 75, 200, 30, 4, 99, 87, 35, 90, 100, 12, 24, 100, 107, 86, 124),
                          Ascites = c(2, 2, 2, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 3, 1, 3, 3, 1, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 3, 1, 1, 2, 1, 1, 3, 2, 1, 1),
                          INR = c(0.96, 1.58, 3.14, 1.53, 1.2, 1.44, 1.29, 1.06, 1.3, 1.32, 1.24, 1.09, 1.18, 1.2, 1.35, 1.24, 1.11, 1.92, 1.34, 2.08, 1.63, 1.23, 1.09, 1.04, 1.93, 1.17, 1.48, 1.46, 1.56, 1.13, 1.24, 1.19, 1.26, 1, 1.39, 1.63, 2.14, 1.27, 1.39, 1.26, 1.46, 1.36, 1.3, 1.62, 1.12, 1.2, 1.57, 1.35, 1.55, 1.33),
                          MCV = c(79.8, 91.5, 107.5, 90.1, 93.8, 103.4, 101, 90.7, 89.5, 96.1, 97.7, 105, 93.8, 91.4, 95.1, 83, 92.4, 119, 81.8, 91.1, 99, 90, 97.6, 98, 93.3, 93.8, 101.5, 103.8, 85.6, 95.1, 96.4, 97.3, 106.3, 94, 93.9, 101.6, 117.3, 95.6, 88, 103.8, 109.5, 88.9, 87.3, 101.8, 92.3, 86.5, 112.2, 95.2, 96.3, 86),
                          Platelets = c(472, 85, 70, 207000, 91000, 101000, 109000, 187, 108, 268000, 170000, 230000, 167000, 275000, 216, 1.71, 270000, 80000, 561, 91000, 75000, 38000, 169000, 77000, 406000, 144000, 120, 53000, 132000, 254000, 280000, 133000, 122, 157000, 88000, 172000, 118000, 272105.73, 174381.93, 175896.57, 68274.47, 114.42, 130783.68, 85246.57, 270585.81, 273354.81, 68596.59, 332033.67, 195.76, 101884.41),
                          Albumin = c(3.3, 3.4, 1.9, 4.4, 4.5, 3.4, 3.6, 4.5, 3, 3.4, 4.2, 4.2, 4.9, 3.11, 2.7, 3.9, 4, 3.1, 2.6, 2.4, 3.5, 2.2, 4.2, 3.5, 2.9, 3.8, 2.2, 3.2, 2.6, 3.68, 4.1, 4.5, 3, 3.88, 2.7, 3.44, 4.8, 3.73, 3.21, 2.43, 2.57, 3.47, 3.79, 3.62, 3.9, 2.89, 2.51, 3.26, 2.93, 3.31),
                          AST = c(68, 122, 59, 36, 96, 87, 35, 47, 85, 29, 85, 26, 29, 94, 523, 28, 73, 357, 43, 145, 85, 51, 31, 192, 266, 74, 71, 87, 219, 38, 52, 63, 401, 51, 73, 95, 60, 124, 20, 114, 86, 80, 60, 184, 75, 67, 106, 56, 68, 48),
                          ALP = c(109, 396, 63, 74, 70, 147, 141, 97, 293, 135, 227, 92, 68, 350, 397, 120, 103, 174, 88, 190, 165, 474, 91, 262, 670, 312, 97, 239, 363, 127, 123, 89, 93, 141, 44, 139, 170, 251, 913, 162, 97, 335, 181, 176, 132, 131, 85, 231, 304, 197),
                          Creatinine = c(2.1, 0.9, 0.59, 0.73, 0.88, 0.9, 0.68, 0.75, 0.67, 0.9, 1.72, 0.8, 0.72, 1.7, 0.82, 0.58, 1.24, 0.99, 0.9, 0.9, 0.7, 2.69, 1.9, 1.2, 4.82, 1.01, 2.82, 0.72, 0.55, 1.11, 0.82, 0.78, 1, 1.1, 0.96, 0.9, 0.74, 1.1, 1.3, 0.68, 0.82, 0.7, 1.17, 0.82, 1.29, 0.61, 0.8, 0.78, 1.07, 1.08),
                          Dir_Bil = c(0.1, 1.4, 1.2, 0.8, 0.2, 1.6, 0.7, 0.2, 0.4, 0.3, 0.3, 0.3, 0.3, 0.8, 5.5, 0.85, 0.2, 4.6, 0.5, 9.6, 1.7, 1, 0.85, 19.5, 29.3, 0.5, 0.3, 1, 1.5, 0.2, 0.5, 0.8, 0.4, 0.33, 1.2, 2.9, 1.8, 1.37, 0.15, 0.56, 3.1, 0.62, 0.75, 2.5, 0.27, 0.25, 1.18, 1.04, 1.57, 0.63),
                          Iron = c(28, 53, 85, 94, 82, 67, 152.6, 87, 94, 59, 104, 52.5, 52.5, 84, 56, 32, 45, 178, 19, 224, 200, 224, 53, 121, 106, 87, 92, 152.6, 40, 28, 131, 78, 124, 94, 37, 111, 161, 50.4, 15.5, 130.5, 50.3, 77.8, 99.1, 95.8, 49.6, 56, 56.9, 69.3, 71.2, 94.4),
                          Sat = c(6, 22, 73, 27, 24, 34, 39, 26, 27, 15, 37, 37, 37, 37, 27, 10, 21, 90, 8, 95, 87, 95, 21, 27, 67, 25, 56, 27, 12, 10, 78, 30, 51, 39, 17, 94, 96, 25, 5, 78, 19, 28, 38, 44, 23, 25, 27, 23, 29, 83),
                          Ferritin = c(16, 111, 982, 70, 80, 774, 76.9, 84, 70, 22, 635, 856, 856, 497, 742, 18, 802, 960, 141, 363, 316, 363, 278, 749, 2165, 81, 48.9, 76.9, 57, 308, 1316, 220, 642, 344, 419, 1600, 297, 828, 147, 1323, 342, 173, 620, 501, 766, 307, 366, 70, 106, 859),
                          Class = c("X1", "X0", "X1", "X1", "X1", "X0", "X1", "X1", "X0", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0"),
                          row.names = c(1, 4, 7, 11, 12, 18, 19, 20, 27, 31, 32, 37, 44, 50, 53, 56, 58, 74, 77, 78, 80, 88, 93, 100, 102, 103, 104, 118, 121, 127, 134, 137, 143, 144, 151, 154, 162, 165, 166, 171, 176, 178, 181, 182, 186, 191, 193, 199, 200, 201))

  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- TwoClass$new(method = "cv",
                             number = 10,
                             savePredictions = "final",
                             classProbs = TRUE,
                             allowParallel = TRUE,
                             verboseIter = FALSE,
                             seed = 1844523989)
  trFunction$create(summaryFunction = UseProbability$new(),
                    search.method = "random")
  metric <- "PPV"
  logs <- normalizePath(path = file.path(tempdir(),
                                         "testModel"),
                        winslash = "/",
                        mustWork = FALSE)
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_type(modelClass$getConfiguration(), "list")
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: save function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "dirpathSave"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_message(modelClass$save(),
                           "[Model][ERROR] Cannot save untrained model. Task not performed",
                           fixed = TRUE)

  train.set <- data.frame(Gender = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1),
                          Hemochro = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          HIV = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          Hallmark = c(1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                          Grams_day = c(50, 100, 100, 60, 0, 500, 200, 80, 60, 100, 100, 100, 100, 100, 100, 100, 0, 100, 80, 100, 100, 100, 100, 0, 0, 0, 75, 180, 75, 0, 0, 0, 100, 100, 250, 75, 200, 30, 4, 99, 87, 35, 90, 100, 12, 24, 100, 107, 86, 124),
                          Ascites = c(2, 2, 2, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 3, 1, 3, 3, 1, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 3, 1, 1, 2, 1, 1, 3, 2, 1, 1),
                          INR = c(0.96, 1.58, 3.14, 1.53, 1.2, 1.44, 1.29, 1.06, 1.3, 1.32, 1.24, 1.09, 1.18, 1.2, 1.35, 1.24, 1.11, 1.92, 1.34, 2.08, 1.63, 1.23, 1.09, 1.04, 1.93, 1.17, 1.48, 1.46, 1.56, 1.13, 1.24, 1.19, 1.26, 1, 1.39, 1.63, 2.14, 1.27, 1.39, 1.26, 1.46, 1.36, 1.3, 1.62, 1.12, 1.2, 1.57, 1.35, 1.55, 1.33),
                          MCV = c(79.8, 91.5, 107.5, 90.1, 93.8, 103.4, 101, 90.7, 89.5, 96.1, 97.7, 105, 93.8, 91.4, 95.1, 83, 92.4, 119, 81.8, 91.1, 99, 90, 97.6, 98, 93.3, 93.8, 101.5, 103.8, 85.6, 95.1, 96.4, 97.3, 106.3, 94, 93.9, 101.6, 117.3, 95.6, 88, 103.8, 109.5, 88.9, 87.3, 101.8, 92.3, 86.5, 112.2, 95.2, 96.3, 86),
                          Platelets = c(472, 85, 70, 207000, 91000, 101000, 109000, 187, 108, 268000, 170000, 230000, 167000, 275000, 216, 1.71, 270000, 80000, 561, 91000, 75000, 38000, 169000, 77000, 406000, 144000, 120, 53000, 132000, 254000, 280000, 133000, 122, 157000, 88000, 172000, 118000, 272105.73, 174381.93, 175896.57, 68274.47, 114.42, 130783.68, 85246.57, 270585.81, 273354.81, 68596.59, 332033.67, 195.76, 101884.41),
                          Albumin = c(3.3, 3.4, 1.9, 4.4, 4.5, 3.4, 3.6, 4.5, 3, 3.4, 4.2, 4.2, 4.9, 3.11, 2.7, 3.9, 4, 3.1, 2.6, 2.4, 3.5, 2.2, 4.2, 3.5, 2.9, 3.8, 2.2, 3.2, 2.6, 3.68, 4.1, 4.5, 3, 3.88, 2.7, 3.44, 4.8, 3.73, 3.21, 2.43, 2.57, 3.47, 3.79, 3.62, 3.9, 2.89, 2.51, 3.26, 2.93, 3.31),
                          AST = c(68, 122, 59, 36, 96, 87, 35, 47, 85, 29, 85, 26, 29, 94, 523, 28, 73, 357, 43, 145, 85, 51, 31, 192, 266, 74, 71, 87, 219, 38, 52, 63, 401, 51, 73, 95, 60, 124, 20, 114, 86, 80, 60, 184, 75, 67, 106, 56, 68, 48),
                          ALP = c(109, 396, 63, 74, 70, 147, 141, 97, 293, 135, 227, 92, 68, 350, 397, 120, 103, 174, 88, 190, 165, 474, 91, 262, 670, 312, 97, 239, 363, 127, 123, 89, 93, 141, 44, 139, 170, 251, 913, 162, 97, 335, 181, 176, 132, 131, 85, 231, 304, 197),
                          Creatinine = c(2.1, 0.9, 0.59, 0.73, 0.88, 0.9, 0.68, 0.75, 0.67, 0.9, 1.72, 0.8, 0.72, 1.7, 0.82, 0.58, 1.24, 0.99, 0.9, 0.9, 0.7, 2.69, 1.9, 1.2, 4.82, 1.01, 2.82, 0.72, 0.55, 1.11, 0.82, 0.78, 1, 1.1, 0.96, 0.9, 0.74, 1.1, 1.3, 0.68, 0.82, 0.7, 1.17, 0.82, 1.29, 0.61, 0.8, 0.78, 1.07, 1.08),
                          Dir_Bil = c(0.1, 1.4, 1.2, 0.8, 0.2, 1.6, 0.7, 0.2, 0.4, 0.3, 0.3, 0.3, 0.3, 0.8, 5.5, 0.85, 0.2, 4.6, 0.5, 9.6, 1.7, 1, 0.85, 19.5, 29.3, 0.5, 0.3, 1, 1.5, 0.2, 0.5, 0.8, 0.4, 0.33, 1.2, 2.9, 1.8, 1.37, 0.15, 0.56, 3.1, 0.62, 0.75, 2.5, 0.27, 0.25, 1.18, 1.04, 1.57, 0.63),
                          Iron = c(28, 53, 85, 94, 82, 67, 152.6, 87, 94, 59, 104, 52.5, 52.5, 84, 56, 32, 45, 178, 19, 224, 200, 224, 53, 121, 106, 87, 92, 152.6, 40, 28, 131, 78, 124, 94, 37, 111, 161, 50.4, 15.5, 130.5, 50.3, 77.8, 99.1, 95.8, 49.6, 56, 56.9, 69.3, 71.2, 94.4),
                          Sat = c(6, 22, 73, 27, 24, 34, 39, 26, 27, 15, 37, 37, 37, 37, 27, 10, 21, 90, 8, 95, 87, 95, 21, 27, 67, 25, 56, 27, 12, 10, 78, 30, 51, 39, 17, 94, 96, 25, 5, 78, 19, 28, 38, 44, 23, 25, 27, 23, 29, 83),
                          Ferritin = c(16, 111, 982, 70, 80, 774, 76.9, 84, 70, 22, 635, 856, 856, 497, 742, 18, 802, 960, 141, 363, 316, 363, 278, 749, 2165, 81, 48.9, 76.9, 57, 308, 1316, 220, 642, 344, 419, 1600, 297, 828, 147, 1323, 342, 173, 620, 501, 766, 307, 366, 70, 106, 859),
                          Class = c("X1", "X0", "X1", "X1", "X1", "X0", "X1", "X1", "X0", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0"),
                          row.names = c(1, 4, 7, 11, 12, 18, 19, 20, 27, 31, 32, 37, 44, 50, 53, 56, 58, 74, 77, 78, 80, 88, 93, 100, 102, 103, 104, 118, 121, 127, 134, 137, 143, 144, 151, 154, 162, 165, 166, 171, 176, 178, 181, 182, 186, 191, 193, 199, 200, 201))

  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- TwoClass$new(method = "cv",
                             number = 10,
                             savePredictions = "final",
                             classProbs = TRUE,
                             allowParallel = TRUE,
                             verboseIter = FALSE,
                             seed = 1844523989)
  trFunction$create(summaryFunction = UseProbability$new(),
                    search.method = "random")
  metric <- "PPV"
  logs <- normalizePath(path = file.path(tempdir(),
                                         "testModel"),
                        winslash = "/",
                        mustWork = FALSE)
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_message(modelClass$save(replace = FALSE),
                           "[Model][INFO][lda] Model succesfully saved at: ",
                           fixed = TRUE)

  testthat::expect_true(file.exists(file.path(dir.path,
                                              "lda.rds")))

  testthat::expect_message(modelClass$save(replace = FALSE),
                           "[Model][INFO][lda] Model already exists. Model not saved",
                           fixed = TRUE)

  testthat::expect_message(modelClass$save(replace = TRUE),
                           "[Model][WARNING][lda] Model already exists. Replacing previous model",
                           fixed = TRUE)
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: remove function works", {

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "testModel",
                                             "dirpathRemove"),
                            winslash = "/",
                            mustWork = FALSE)

  model <- data.frame(name = c("lda"),
                      description = c("Linear Discriminant Analysis"),
                      family = c("Discriminant Analysis"),
                      library = c("MASS"),
                      prob = c(TRUE),
                      row.names = c(63))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_message(modelClass$save(),
                           "[Model][ERROR] Cannot save untrained model. Task not performed",
                           fixed = TRUE)

  train.set <- data.frame(Gender = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1),
                          Hemochro = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          HIV = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          Hallmark = c(1, 1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                          Grams_day = c(50, 100, 100, 60, 0, 500, 200, 80, 60, 100, 100, 100, 100, 100, 100, 100, 0, 100, 80, 100, 100, 100, 100, 0, 0, 0, 75, 180, 75, 0, 0, 0, 100, 100, 250, 75, 200, 30, 4, 99, 87, 35, 90, 100, 12, 24, 100, 107, 86, 124),
                          Ascites = c(2, 2, 2, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 3, 1, 3, 3, 1, 2, 2, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 3, 1, 1, 2, 1, 1, 3, 2, 1, 1),
                          INR = c(0.96, 1.58, 3.14, 1.53, 1.2, 1.44, 1.29, 1.06, 1.3, 1.32, 1.24, 1.09, 1.18, 1.2, 1.35, 1.24, 1.11, 1.92, 1.34, 2.08, 1.63, 1.23, 1.09, 1.04, 1.93, 1.17, 1.48, 1.46, 1.56, 1.13, 1.24, 1.19, 1.26, 1, 1.39, 1.63, 2.14, 1.27, 1.39, 1.26, 1.46, 1.36, 1.3, 1.62, 1.12, 1.2, 1.57, 1.35, 1.55, 1.33),
                          MCV = c(79.8, 91.5, 107.5, 90.1, 93.8, 103.4, 101, 90.7, 89.5, 96.1, 97.7, 105, 93.8, 91.4, 95.1, 83, 92.4, 119, 81.8, 91.1, 99, 90, 97.6, 98, 93.3, 93.8, 101.5, 103.8, 85.6, 95.1, 96.4, 97.3, 106.3, 94, 93.9, 101.6, 117.3, 95.6, 88, 103.8, 109.5, 88.9, 87.3, 101.8, 92.3, 86.5, 112.2, 95.2, 96.3, 86),
                          Platelets = c(472, 85, 70, 207000, 91000, 101000, 109000, 187, 108, 268000, 170000, 230000, 167000, 275000, 216, 1.71, 270000, 80000, 561, 91000, 75000, 38000, 169000, 77000, 406000, 144000, 120, 53000, 132000, 254000, 280000, 133000, 122, 157000, 88000, 172000, 118000, 272105.73, 174381.93, 175896.57, 68274.47, 114.42, 130783.68, 85246.57, 270585.81, 273354.81, 68596.59, 332033.67, 195.76, 101884.41),
                          Albumin = c(3.3, 3.4, 1.9, 4.4, 4.5, 3.4, 3.6, 4.5, 3, 3.4, 4.2, 4.2, 4.9, 3.11, 2.7, 3.9, 4, 3.1, 2.6, 2.4, 3.5, 2.2, 4.2, 3.5, 2.9, 3.8, 2.2, 3.2, 2.6, 3.68, 4.1, 4.5, 3, 3.88, 2.7, 3.44, 4.8, 3.73, 3.21, 2.43, 2.57, 3.47, 3.79, 3.62, 3.9, 2.89, 2.51, 3.26, 2.93, 3.31),
                          AST = c(68, 122, 59, 36, 96, 87, 35, 47, 85, 29, 85, 26, 29, 94, 523, 28, 73, 357, 43, 145, 85, 51, 31, 192, 266, 74, 71, 87, 219, 38, 52, 63, 401, 51, 73, 95, 60, 124, 20, 114, 86, 80, 60, 184, 75, 67, 106, 56, 68, 48),
                          ALP = c(109, 396, 63, 74, 70, 147, 141, 97, 293, 135, 227, 92, 68, 350, 397, 120, 103, 174, 88, 190, 165, 474, 91, 262, 670, 312, 97, 239, 363, 127, 123, 89, 93, 141, 44, 139, 170, 251, 913, 162, 97, 335, 181, 176, 132, 131, 85, 231, 304, 197),
                          Creatinine = c(2.1, 0.9, 0.59, 0.73, 0.88, 0.9, 0.68, 0.75, 0.67, 0.9, 1.72, 0.8, 0.72, 1.7, 0.82, 0.58, 1.24, 0.99, 0.9, 0.9, 0.7, 2.69, 1.9, 1.2, 4.82, 1.01, 2.82, 0.72, 0.55, 1.11, 0.82, 0.78, 1, 1.1, 0.96, 0.9, 0.74, 1.1, 1.3, 0.68, 0.82, 0.7, 1.17, 0.82, 1.29, 0.61, 0.8, 0.78, 1.07, 1.08),
                          Dir_Bil = c(0.1, 1.4, 1.2, 0.8, 0.2, 1.6, 0.7, 0.2, 0.4, 0.3, 0.3, 0.3, 0.3, 0.8, 5.5, 0.85, 0.2, 4.6, 0.5, 9.6, 1.7, 1, 0.85, 19.5, 29.3, 0.5, 0.3, 1, 1.5, 0.2, 0.5, 0.8, 0.4, 0.33, 1.2, 2.9, 1.8, 1.37, 0.15, 0.56, 3.1, 0.62, 0.75, 2.5, 0.27, 0.25, 1.18, 1.04, 1.57, 0.63),
                          Iron = c(28, 53, 85, 94, 82, 67, 152.6, 87, 94, 59, 104, 52.5, 52.5, 84, 56, 32, 45, 178, 19, 224, 200, 224, 53, 121, 106, 87, 92, 152.6, 40, 28, 131, 78, 124, 94, 37, 111, 161, 50.4, 15.5, 130.5, 50.3, 77.8, 99.1, 95.8, 49.6, 56, 56.9, 69.3, 71.2, 94.4),
                          Sat = c(6, 22, 73, 27, 24, 34, 39, 26, 27, 15, 37, 37, 37, 37, 27, 10, 21, 90, 8, 95, 87, 95, 21, 27, 67, 25, 56, 27, 12, 10, 78, 30, 51, 39, 17, 94, 96, 25, 5, 78, 19, 28, 38, 44, 23, 25, 27, 23, 29, 83),
                          Ferritin = c(16, 111, 982, 70, 80, 774, 76.9, 84, 70, 22, 635, 856, 856, 497, 742, 18, 802, 960, 141, 363, 316, 363, 278, 749, 2165, 81, 48.9, 76.9, 57, 308, 1316, 220, 642, 344, 419, 1600, 297, 828, 147, 1323, 342, 173, 620, 501, 766, 307, 366, 70, 106, 859),
                          Class = c("X1", "X0", "X1", "X1", "X1", "X0", "X1", "X1", "X0", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X0", "X0", "X1", "X1", "X1", "X1", "X1", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0", "X0"),
                          row.names = c(1, 4, 7, 11, 12, 18, 19, 20, 27, 31, 32, 37, 44, 50, 53, 56, 58, 74, 77, 78, 80, 88, 93, 100, 102, 103, 104, 118, 121, 127, 134, 137, 143, 144, 151, 154, 162, 165, 166, 171, 176, 178, 181, 182, 186, 191, 193, 199, 200, 201))

  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- TwoClass$new(method = "cv",
                             number = 10,
                             savePredictions = "final",
                             classProbs = TRUE,
                             allowParallel = TRUE,
                             verboseIter = FALSE,
                             seed = 1844523989)
  trFunction$create(summaryFunction = UseProbability$new(),
                    search.method = "random")
  metric <- "PPV"
  logs <- normalizePath(path = file.path(tempdir(),
                                         "testModel"),
                        winslash = "/",
                        mustWork = FALSE)
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_message(modelClass$remove(),
                           "[Model][ERROR] Cannot remove unsaved model. Task not performed",
                           fixed = TRUE)

  testthat::expect_message(modelClass$save(replace = FALSE),
                           "[Model][INFO][lda] Model succesfully saved at: ",
                           fixed = TRUE)

  testthat::expect_true(file.exists(file.path(dir.path,
                                              "lda.rds")))

  modelClass$remove()

  testthat::expect_false(file.exists(file.path(dir.path,
                                               "lda.rds")))
})

testthat::teardown({
  if (file.exists(normalizePath(path = file.path(tempdir(),
                                                 "testModel",
                                                 "dirpathRemove"),
                                winslash = "/",
                                mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "testModel",
                                              "dirpathRemove"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})
