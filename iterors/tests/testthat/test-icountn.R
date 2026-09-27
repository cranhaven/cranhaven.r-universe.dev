`%is%` <- expect_equal

test_that("icountn", {
  xcountn <- function(x) {
    iteror(do.call('expand.grid', lapply(x, \(x)0+seq_len(x))), by='row')
  }

  vv <- list(0, 1, 2, 10, 100,
             c(0, 1), c(0, 2), c(3, 0),
             c(1, 1), c(1, 2), c(1, 3),
             c(2, 1), c(2, 2), c(2, 3),
             c(10, 10, 0, 10),
             c(1, 1, 2, 1, 1, 3, 1, 1, 1, 2, 1, 1, 1),
             c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
             c(10, 10, 10, 10))

  expect_silent(for (v in vv) {
    ait <- icountn(v)
    xit <- xcountn(v)
    cit <- i_map(actual=ait, expected=xit, f=function(actual, expected) {
      same <- identical(actual, unname(unlist(expected)))
      if (!same ) {
        expect_equal(actual, unname(unlist(expected)))
      }
      same
    })
    repeat nextOr(cit, break)
  })
})

test_that("icountn chunking", {
  smooth <- icountn(c(x=3, y=7, z=9))
  chunky <- icountn(c(x=3, y=7, z=9), chunkSize=10)
  chunked <- icountn(c(x=3, y=7, z=9), chunks=15)
  actual <- do.call(rbind, as.list(chunky))
  actualChunks <- do.call(rbind, as.list(chunked))
  expected <- do.call(rbind, as.list(smooth))
  expect_equal(actual, expected)
  expect_equal(actualChunks, expected)
})

test_that("icountn respects names", {
  it <- icountn(c(x=2, y=2))
  nextOr(it) %is% c(x=1, y=1)
  nextOr(it) %is% c(x=2, y=1)
  nextOr(it) %is% c(x=1, y=2)
  nextOr(it) %is% c(x=2, y=2)
})

test_that("icountn row-major", {
  it <- icountn(c(x=2, y=2), rowMajor=FALSE)
  nextOr(it) %is% c(x=1, y=1)
  nextOr(it) %is% c(x=1, y=2)
  nextOr(it) %is% c(x=2, y=1)
  nextOr(it) %is% c(x=2, y=2)
})
