test_that("block matrix multiplication", {

 library(foreach)

  n <- 10777
  nrowsx <- 959
  ncolsy <- 101
  chunks <- 80  # applied to "n"
  xchunks <- 2  # applied to "nrowsx"
  ychunks <- 2  # applied to "ncolsy"

  x <- matrix(rnorm(nrowsx * n), nrow=nrowsx)
  y <- matrix(rnorm(n * ncolsy), nrow=n)
  expected <- x %*% y

  ## test <- array(seq_len(27 * 31), c(27, 31))
  ## theirs <- iarray(test, c(2,1), chunks=c(4, 3))
  ## ours <- iteror(test, by=1, chunks=3) |> i_apply(iteror, by=2, chunks=4)
  ## ou <- ours()
  ## th <- theirs()

  # This uses/demonstrates the "nesting" functionality of foreach.
  # TODO: think of a better way to express blockwise matrix iteration

  actual <- foreach(ia=(iteror(x, by=1, chunks=xchunks) |>
                          i_apply(iteror, by=2, chunks=chunks)),
                    .combine=rbind) %:%
    foreach(a=ia, ib = (iteror(y, by=1, chunks=chunks) |>
                          i_apply(iteror, by=2, chunks=ychunks)),
            .combine=`+`) %:%
    foreach(b=ib, .combine=cbind) %do% {
      a %*% b
    }
  expect_equal(actual, expected)

  actual <- foreach(ib=(iteror(y, by=2, chunks=ychunks) |>
                          i_apply(iteror, by=1, chunks=chunks)),
                    .combine=cbind) %:%
    foreach(b=ib, ia=(iteror(x, by=2, chunks=chunks) |>
                        i_apply(iteror, by=1, chunks=xchunks)),
            .combine=`+`) %:%
    foreach(a=ia, .combine=rbind) %do% {
      a %*% b
    }
  expect_equal(actual, expected)

})
