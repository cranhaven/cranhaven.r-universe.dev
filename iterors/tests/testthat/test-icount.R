`%is%` <- expect_equal

test_that("icount's default values generate the sequence 1, 2, ..", {
  it <- icount()

  i <- nextOr(it, NULL)
  expect_equal(i, 1)

  i <- nextOr(it, NULL)
  expect_equal(i, 2)

  i <- nextOr(it, NULL)
  expect_equal(i, 3)

  i <- nextOr(it, NULL)
  expect_equal(i, 4)
})

test_that("iseq works with a given initial value", {
  it <- iseq(from=42)

  i <- nextOr(it, NULL)
  expect_equal(i, 42)

  i <- nextOr(it, NULL)
  expect_equal(i, 43)

  i <- nextOr(it, NULL)
  expect_equal(i, 44)

  i <- nextOr(it, NULL)
  expect_equal(i, 45)
})

test_that("iseq works with a decimal step size", {
  it <- iseq(from=42, by=1.5)

  i <- nextOr(it, NULL)
  expect_equal(i, 42)

  i <- nextOr(it, NULL)
  expect_equal(i, 43.5)

  i <- nextOr(it, NULL)
  expect_equal(i, 45)

  i <- nextOr(it, NULL)
  expect_equal(i, 46.5)
})

test_that("iseq works with the optional stop", {
  it <- iseq(from=0, to=5)
  expect_equal(0:5, unlist(as.list(it)))
})

test_that("iseq works with a given initial value and a stop", {
  it <- iseq(from=42, to=50)
  expect_equal(42:50, unlist(as.list(it)))
})

test_that("iseq works with a stop and decimal step size", {
  it <- iseq(from=42, by=1.5, to=50)
  expect_equal(seq(42, 50, by=1.5), unlist(as.list(it)))
})

test_that("icount respects names", {
  it <- icount(c(name=2), recycle=TRUE)
  nextOr(it) %is% c(name=1)
  nextOr(it) %is% c(name=2)
  nextOr(it) %is% c(name=1)

  it <- icount(c(x=Inf))
  nextOr(it) %is% c(x=1)
  nextOr(it) %is% c(x=2)
})

test_that("iseq respects names", {
  it <- iseq(from=c(x=42), by=4, to=50, recycle=TRUE)
  nextOr(it) %is% c(x=42)
  nextOr(it) %is% c(x=46)
  nextOr(it) %is% c(x=50)
  nextOr(it) %is% c(x=42)
})

test_that("icount chunks", {
  for (chunks in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 101)) {
    l <- as.list(icount(100, chunks = chunks))
      expect_length(l, min(chunks, 100))
      y <- c(l, recursive=TRUE)
      expect_equal(y, 1:100)
  }
  for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 101)) {
    l <- as.list(icount(100, chunkSize = chunkSize))
      expect_length(l[[1]], min(chunkSize, 100))
      y <- c(l, recursive=TRUE)
      expect_equal(y, 1:100)
  }
  for (chunkSize in c(1, 2, 3, 4, 9, 10, 19, 20, 99, 100, 101)) {
    l <- take(icount(Inf, chunkSize=chunkSize), 10)
    expect_length(l, 10)
    expect_equal(c(l, recursive=TRUE), seq_len(10*chunkSize))
  }
})
