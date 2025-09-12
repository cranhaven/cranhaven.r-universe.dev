testthat::test_that("DatasetLoader: initialize function works", {
  data.loader <- DatasetLoader$new()
  testthat::expect_is(data.loader, "DatasetLoader")
})

testthat::test_that("DatasetLoader: load function checks parameter type", {

  data.loader <- DatasetLoader$new()

  testthat::expect_error(data.loader$load(filepath = "wrongFile.csv", header = TRUE,
                                          sep = ",", skip.lines = 1, normalize.names = TRUE,
                                          string.as.factor = FALSE, ignore.columns = NULL),
                         "[DatasetLoader][FATAL] Corpus cannot be found at defined location. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(data.loader$load(filepath = NULL, header = TRUE, sep = ",",
                                          skip.lines = 1, normalize.names = TRUE,
                                          string.as.factor = FALSE, ignore.columns = NULL),
                         "[DatasetLoader][FATAL] Corpus cannot be found at defined location. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("DatasetLoader: load function works", {

  data.loader <- DatasetLoader$new()
  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- data.loader$load(filepath = file.path, header = TRUE, sep = ",",
                           skip.lines = 1, normalize.names = TRUE,
                           string.as.factor = FALSE, ignore.columns = NULL)

  testthat::expect_is(data, "Dataset")
})
