test_that("i_enumerate over numeric vector", {
  set.seed(42)
  x <- rnorm(5)

  it <- i_enumerate(x)

  i <- nextOr(it, NA)
  expect_equal(i$index, 1)
  expect_equal(i$value, x[1])

  i <- nextOr(it, NA)
  expect_equal(i$index, 2)
  expect_equal(i$value, x[2])

  i <- nextOr(it, NA)
  expect_equal(i$index, 3)
  expect_equal(i$value, x[3])

  i <- nextOr(it, NA)
  expect_equal(i$index, 4)
  expect_equal(i$value, x[4])

  i <- nextOr(it, NA)
  expect_equal(i$index, 5)
  expect_equal(i$value, x[5])

  expect_equal(nextOr(it, NA), NA)
})

test_that("i_enumerate chunks", {
  expected <- do.call(rbind, as.list(
    i_apply(i_enumerate(seq(1, 1000, by=3)),
           as.data.frame)))
  actual <- do.call(rbind, as.list(
    i_apply(i_enumerate(seq(1, 1000, by=3), chunkSize=5),
           as.data.frame)))
  expect_equal(expected, actual)
  actual2 <- do.call(rbind, as.list(
    i_apply(i_enumerate(seq(1, 1000, by=3), chunks=12),
           as.data.frame)))
  expect_equal(expected, actual2)
})


test_that("i_enum over numeric vector", {
  set.seed(42)
  x <- rnorm(5)

  it <- i_enum(x)

  i <- nextOr(it, NA)
  expect_equal(i$index, 1)
  expect_equal(i$value, x[1])

  i <- nextOr(it, NA)
  expect_equal(i$index, 2)
  expect_equal(i$value, x[2])

  i <- nextOr(it, NA)
  expect_equal(i$index, 3)
  expect_equal(i$value, x[3])

  i <- nextOr(it, NA)
  expect_equal(i$index, 4)
  expect_equal(i$value, x[4])

  i <- nextOr(it, NA)
  expect_equal(i$index, 5)
  expect_equal(i$value, x[5])

  expect_equal(nextOr(it, NA), NA)
})

test_that("i_enumerate split array and rejoin", {

  input <- array(1:24, c(2,3,4))

  for (by in list(1, 2, 3, c(1, 2), c(2,3), c(1, 3), c(3, 1), c(3, 2))) {
    slices <- as.list(i_enum(input, by=by))
    out <- array(0, c(2, 3, 4))
    ix <- alist( , , )
    d <- dim(input)
    d[by] <- 1
    for (slice in slices) {
      ix[by] <- slice$index
      expect_equal(dim(slice$value), d)
      out <- do.call(`[<-`, c(list(out), ix, list(slice$value)))
    }
    expect_equal(out, input)
  }

  for (by in list(1, 2, 3, c(1, 2), c(2,3), c(1, 3), c(3, 1), c(3, 2))) {
    slices <- as.list(i_enum(input, by=by, drop=TRUE))
    out <- array(0, c(2, 3, 4))
    ix <- alist( , , )
    d <- dim(input)
    d <- d[-by]
    for (slice in slices) {
      ix[by] <- slice$index
      if (length(d) > 1)
        expect_equal(dim(slice$value), d)
      else expect_equal(length(slice$value), d)
      out <- do.call(`[<-`, c(list(out), ix, list(slice$value)))
    }
    expect_equal(out, input)
  }

})

test_that("i_enumerate chunked vector", {
  actual <- nextOr(i_enumerate(letters, chunks=4))
  expected <- list(index=1:7, value=letters[1:7])
  expect_equal(actual, expected)
})

test_that("i_enumerate chunked array", {

  input <- array(1:504, c(7,8,9))

  for (by in list(1, 2, 3, c(1, 2), c(2,3), c(1, 3), c(3, 1), c(3, 2))) {
    slices <- as.list(i_enum(input, by=by, chunkSize=5))
    out <- array(0, c(7, 8, 9))
    ix_in <- alist( , , )
    ix_out <- ix_in
    d <- dim(input)
    d[by] <- 1
    for (slice in slices) {
      d[by[1]] <- nrow(slice$index)
      expect_equal(dim(slice$value), d)
      for (i in seq_len(nrow(slice$index))) {
        ix_in[by[1]] <- i
        ix_out[by] <- slice$index[i,]
        #cat(deparse(list(ix_in=ix_in, ix_out=ix_out)), "\n")
        subslice <- do.call("[", c(list(slice$value), ix_in))
        out <- do.call(`[<-`, c(list(out), ix_out, list(subslice)))
      }
    }
    expect_equal(out, input)
  }

})

test_that("i_enumerate over data.frame", {
  it <- i_enumerate(iris)

  i <- nextOr(it, NA)
  expect_equal(i$index, 1)
  expect_equal(i$value, iris[, 1])

  i <- nextOr(it, NA)
  expect_equal(i$index, 2)
  expect_equal(i$value, iris[, 2])

  i <- nextOr(it, NA)
  expect_equal(i$index, 3)
  expect_equal(i$value, iris[, 3])

  i <- nextOr(it, NA)
  expect_equal(i$index, 4)
  expect_equal(i$value, iris[, 4])

  i <- nextOr(it, NA)
  expect_equal(i$index, 5)
  expect_equal(i$value, iris[, 5])

  expect_equal(nextOr(it, NA), NA)
})

test_that("i_enum over data.frame", {
  it <- i_enum(iris)

  i <- nextOr(it, NA)
  expect_equal(i$index, 1)
  expect_equal(i$value, iris[, 1])

  i <- nextOr(it, NA)
  expect_equal(i$index, 2)
  expect_equal(i$value, iris[, 2])

  i <- nextOr(it, NA)
  expect_equal(i$index, 3)
  expect_equal(i$value, iris[, 3])

  i <- nextOr(it, NA)
  expect_equal(i$index, 4)
  expect_equal(i$value, iris[, 4])

  i <- nextOr(it, NA)
  expect_equal(i$index, 5)
  expect_equal(i$value, iris[, 5])

  expect_equal(nextOr(it, NA), NA)
})
