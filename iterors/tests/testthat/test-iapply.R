test_that("iterate over 3D arrays", {

  test <- function(actual, it) {
    expected <- nextOr(it)
    expect_equal(expected, actual)
    NULL
  }

  a <- array(1:24, c(2,3,4))

  margins <- list(1, 2, 3,
                  c(1, 2), c(1, 3), c(2, 1), c(2, 3), c(3, 1), c(3, 2),
                  c(1, 2, 3), c(1, 3, 2), c(2, 1, 3), c(2, 3, 1),
                  c(3, 1, 2), c(3, 2, 1))
  for(MARGIN in margins) {
    # cat(sprintf('testing %s\n', paste(MARGIN, collapse=', ')))
    it <- iteror(a, by=MARGIN, drop=TRUE)
    apply(a, MARGIN, test, it)
  }

})

test_that("i_apply on matrices", {
  test <- function(actual, it) {
    expected <- nextOr(it)
    expect_equal(expected, actual)
    NULL
  }

  m <- matrix(1:24, c(6,4))

  margins <- list(1, 2, c(1, 2), c(2, 1))
  for(MARGIN in margins) {
    # cat(sprintf('testing %s\n', paste(MARGIN, collapse=', ')))
    it <- iteror(m, by=MARGIN, drop=TRUE)
    apply(m, MARGIN, test, it)
  }
})

test_that("chunk rows", {

  x <- matrix(rnorm(10000), 100)

    for (chunks in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 101)) {
      l <- as.list(iteror(x, by="row", chunks = chunks))
      expect_length(l, min(chunks, 100))
      y <- do.call("rbind", l)
      expect_identical(y, x)
    }

    for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 101)) {
      l <- as.list(iteror(x, by="row", chunkSize = chunkSize))
      l <- as.list(iteror(x, by="row", chunks = chunks))
      y <- do.call("rbind", as.list(iteror(x, by="row", chunkSize = chunkSize)))
        expect_identical(y, x)
    }

})

test_that("chunk cols", {

  x <- matrix(rnorm(10000), 100)
    for (chunks in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 101)) {
        y <- do.call("cbind", as.list(iteror(x, by=2, chunks = chunks)))
        expect_identical(y, x)
    }
    for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 101)) {
        y <- do.call("cbind", as.list(iteror(x, by=2, chunkSize = chunkSize)))
        expect_identical(y, x)
    }
})

test_that("chunk counts", {
    x <- 0 + seq(length = 1000)
    for (chunks in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999,
        1000, 1001)) {
        y <- do.call("c", as.list(icount(length(x), chunks = chunks)))
        expect_identical(y, x)
    }
    for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999,
        1000, 1001)) {
        y <- do.call("c", as.list(icount(length(x), chunkSize = chunkSize)))
        expect_identical(y, x)
    }
})
