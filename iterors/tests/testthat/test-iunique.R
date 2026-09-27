`%is%` <- expect_equal

test_that("i_unique works with numeric vectors", {
  x <- rep(1:5, each=10)
  it_unique <- i_unique(x)
  expect_equal(take(it_unique, 5), as.list(1:5))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("i_unique works with character vectors", {
  x <- as.character(gl(5, 10, labels=LETTERS[1:5]))
  it_unique <- i_unique(x)
  expect_equal(take(it_unique, 5), as.list(LETTERS[1:5]))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("i_unique works with iterators from numeric vectors", {
  x <- rep(1:5, each=10)
  it <- iteror(rep(x, 2))
  it_unique <- i_unique(it)
  expect_equal(take(it_unique, 5), as.list(1:5))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("i_unique works with iterators from character vectors", {
  x <- as.character(gl(5, 10, labels=LETTERS[1:5]))
  it <- iteror(rep(x, 2))
  it_unique <- i_unique(it)
  expect_equal(take(it_unique, 5), as.list(LETTERS[1:5]))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("idedup works with numeric vectors", {
  x <- rep(1:5, each=10)
  it_unique <- idedup(x)
  expect_equal(take(it_unique, 5), as.list(1:5))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("i_dedupe works with character vectors", {
  x <- as.character(gl(5, 10, labels=LETTERS[1:5]))
  it_unique <- idedup(x)
  expect_equal(take(it_unique, 5), as.list(LETTERS[1:5]))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("i_dedupe works with iterators from numeric vectors", {
  num_reps <- 7
  x <- rep(1:5, each=10)
  it <- iteror(rep(x, num_reps))
  it_unique <- idedup(it)
  expect_equal(take(it_unique, 5 * num_reps), as.list(rep(1:5, num_reps)))
  expect_equal(nextOr(it_unique, NA), NA)
})

test_that("i_dedupe works with iterators from character vectors", {
  num_reps <- 7
  x <- as.character(gl(5, 10, labels=LETTERS[1:5]))
  it <- iteror(rep(x, num_reps))
  it_unique <- idedup(it)
  expect_equal(take(it_unique, 5 * num_reps), as.list(rep(LETTERS[1:5], num_reps)))
  expect_equal(nextOr(it_unique, NA), NA)
})
