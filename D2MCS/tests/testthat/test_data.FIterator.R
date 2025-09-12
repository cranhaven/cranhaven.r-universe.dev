testthat::test_that("FIterator: initialize function works", {

  file.path <- file.path("resourceFiles",
                        "data",
                        "hcc-data-complete-balanced.csv")

  sep <- ","
  column.names <- unlist(strsplit(scan(file = file.path, nlines = 1,
                                       what = "character", quiet = TRUE),
                                  split = sep))
  feature.names <- setNames(data.frame(matrix(ncol = length(column.names),
                             nrow = 0)),
                            column.names)

  config.params <- list(file.path = file.path,
                        feature.names = feature.names,
                        start = 0, sep = sep)

  chunk.size <- 1
  verbose <- TRUE

  fIterator <- FIterator$new(config.params = config.params,
                             chunk.size = chunk.size,
                             verbose = verbose)
  testthat::expect_is(fIterator,
                      "FIterator")
})

testthat::test_that("FIterator: getNext function works", {

  file.path <- file.path("resourceFiles",
                         "data",
                         "hcc-data-complete-balanced.csv")

  sep <- ","
  column.names <- unlist(strsplit(scan(file = file.path, nlines = 1,
                                       what = "character", quiet = TRUE),
                                  split = sep))
  feature.names <- setNames(data.frame(matrix(ncol = length(column.names),
                                              nrow = 0)),
                            column.names)

  config.params <- list(file.path = file.path,
                        feature.names = feature.names,
                        start = 0, sep = sep)

  chunk.size <- 1000000
  verbose <- FALSE

  fIterator <- FIterator$new(config.params = config.params,
                             chunk.size = chunk.size,
                             verbose = verbose)

  fIterator$getNext()
  testthat::expect_null(fIterator$getNext())

  chunk.size <- 100
  verbose <- FALSE

  fIterator <- FIterator$new(config.params = config.params,
                             chunk.size = chunk.size,
                             verbose = verbose)

  fIterator$getNext()
  testthat::expect_is(fIterator$getNext(),
                      "data.frame")

  chunk.size <- 100
  verbose <- TRUE

  fIterator <- FIterator$new(config.params = config.params,
                             chunk.size = chunk.size,
                             verbose = verbose)

  fIterator$getNext()
  testthat::expect_message(fIterator$getNext(),
                           "[FIterator][INFO] Read lines 100 to 200 [100]",
                           fixed = TRUE)
})

testthat::test_that("FIterator: isLast function works", {

  file.path <- file.path("resourceFiles",
                         "data",
                         "hcc-data-complete-balanced.csv")

  sep <- ","
  column.names <- unlist(strsplit(scan(file = file.path, nlines = 1,
                                       what = "character", quiet = TRUE),
                                  split = sep))
  feature.names <- setNames(data.frame(matrix(ncol = length(column.names),
                                              nrow = 0)),
                            column.names)

  config.params <- list(file.path = file.path,
                        feature.names = feature.names,
                        start = 0, sep = sep)

  chunk.size <- 1000000
  verbose <- TRUE

  fIterator <- FIterator$new(config.params = config.params,
                             chunk.size = chunk.size,
                             verbose = verbose)

  fIterator$getNext()
  testthat::expect_true(fIterator$isLast())
})

testthat::test_that("FIterator: finalize function works", {

  file.path <- file.path("resourceFiles",
                         "data",
                         "hcc-data-complete-balanced.csv")

  sep <- ","
  column.names <- unlist(strsplit(scan(file = file.path, nlines = 1,
                                       what = "character", quiet = TRUE),
                                  split = sep))
  feature.names <- setNames(data.frame(matrix(ncol = length(column.names),
                                              nrow = 0)),
                            column.names)

  config.params <- list(file.path = file.path,
                        feature.names = feature.names,
                        start = 0, sep = sep)

  chunk.size <- 1000000
  verbose <- TRUE

  fIterator <- FIterator$new(config.params = config.params,
                             chunk.size = chunk.size,
                             verbose = verbose)
  fIterator$finalize()

  testthat::expect_null(fIterator$.__enclos_env__$private$con)
})
