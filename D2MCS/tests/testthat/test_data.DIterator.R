testthat::test_that("DIterator: initialize function works", {

  data <- data.frame(c(1, 0, 1), c(0, 0, 1))
  chunk.size <- 1
  verbose <- TRUE

  testthat::expect_is(DIterator$new(data = data,
                                    chunk.size = chunk.size,
                                    verbose = verbose),
                      "DIterator")
})

testthat::test_that("DIterator: getNext function works", {

  data <- data.frame(c(1, 0, 1), c(0, 0, 1))
  chunk.size <- 4
  verbose <- FALSE

  dIterator  <- DIterator$new(data = data,
                              chunk.size = chunk.size,
                              verbose = verbose)

  dIterator$getNext()
  testthat::expect_null(dIterator$getNext())

  data <- data.frame(c(1, 0, 1), c(0, 0, 1))
  chunk.size <- 2
  verbose <- FALSE

  dIterator  <- DIterator$new(data = data,
                              chunk.size = chunk.size,
                              verbose = verbose)
  testthat::expect_is(dIterator$getNext(),
                      "data.frame")

  data <- data.frame(c(1, 0, 1), c(0, 0, 1))
  chunk.size <- 1
  verbose <- TRUE

  dIterator  <- DIterator$new(data = data,
                              chunk.size = chunk.size,
                              verbose = verbose)
  testthat::expect_message(dIterator$getNext(),
                           "[DIterator][INFO] Read lines 0 to 1 [1]",
                           fixed = TRUE)
})

testthat::test_that("DIterator: isLast function works", {

  data <- data.frame(c(1, 0, 1), c(0, 0, 1))
  chunk.size <- 4
  verbose <- FALSE

  dIterator  <- DIterator$new(data = data,
                              chunk.size = chunk.size,
                              verbose = verbose)

  dIterator$getNext()
  testthat::expect_true(dIterator$isLast())
})
