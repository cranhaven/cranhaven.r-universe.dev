library(iterors)


test_that("chunk vectors", {
  vecs <- list(as.list(rnorm(1000)),
               rnorm(1000),
               as.character(rnorm(1000)))

  for (x in vecs) {
    x <- as.list(rnorm(1000))
    for (chunks in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999,
                     1000, 1001)) {
      y <- do.call("c", as.list(iteror(x, chunks = chunks)))
      expect_identical(y, x)
    }
    for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 999,
                        1000, 1001)) {
      y <- do.call("c", as.list(iteror(x, chunkSize = chunkSize)))
      expect_identical(y, x)
    }
  }

})

test_that("reject unused args", {
  x <- as.character(rnorm(1000))
  expect_error(iteror(x, chunksize = 12), "unused argument")
})

test_that("test that various values of chunkSize", {

  nr <- 13
  nc <- 21
  mat <- matrix(rnorm(nr * nc), nr)

  for (n in 1:(nc+2)) {
    it <- iteror(mat, by='col', chunkSize=n)
    bcols <- as.list(it)
    for (bcol in bcols) {
      expect_true(nrow(bcol) == nr)
      expect_true(ncol(bcol) <= n && ncol(bcol) >= 1)
    }
    actual <- do.call('cbind', bcols)
    expect_identical(mat, actual)
  }

  for (n in 1:(nr+2)) {
    it <- iteror(mat, by='row', chunkSize=n)
    brows <- as.list(it)
    for (brow in brows) {
      expect_true(ncol(brow) == nc)
      expect_true(nrow(brow) <= n && nrow(brow) >= 1)
    }
    actual <- do.call('rbind', brows)
    expect_identical(mat, actual)
  }

})
