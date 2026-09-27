`%is%` <- expect_equal


test_that("igrid constructs the Cartesian product of two unnamed numeric vectors", {
  it <- igrid(1:3, 4:6, rowMajor=FALSE)
  expect_equal(nextOr(it, NA), list(1, 4))
  expect_equal(nextOr(it, NA), list(1, 5))
  expect_equal(nextOr(it, NA), list(1, 6))
  expect_equal(nextOr(it, NA), list(2, 4))
  expect_equal(nextOr(it, NA), list(2, 5))
  expect_equal(nextOr(it, NA), list(2, 6))
  expect_equal(nextOr(it, NA), list(3, 4))
  expect_equal(nextOr(it, NA), list(3, 5))
  expect_equal(nextOr(it, NA), list(3, 6))
  expect_equal(nextOr(it, NA), NA)
})

test_that("igrid constructs the Cartesian product of three unnamed numeric vectors", {
  it <- igrid(1:2, 3:4, 5:6, rowMajor=FALSE)
  expect_equal(nextOr(it, NA), list(1, 3, 5))
  expect_equal(nextOr(it, NA), list(1, 3, 6))
  expect_equal(nextOr(it, NA), list(1, 4, 5))
  expect_equal(nextOr(it, NA), list(1, 4, 6))
  expect_equal(nextOr(it, NA), list(2, 3, 5))
  expect_equal(nextOr(it, NA), list(2, 3, 6))
  expect_equal(nextOr(it, NA), list(2, 4, 5))
  expect_equal(nextOr(it, NA), list(2, 4, 6))

  expect_equal(nextOr(it, NA), NA)
})

test_that("igrid constructs the Cartesian product of two named numeric vectors", {
  it <- igrid(a=1:3, b=4:6)
  expect_equal(nextOr(it, NA), list(a=1, b=4))
  expect_equal(nextOr(it, NA), list(a=2, b=4))
  expect_equal(nextOr(it, NA), list(a=3, b=4))
  expect_equal(nextOr(it, NA), list(a=1, b=5))
  expect_equal(nextOr(it, NA), list(a=2, b=5))
  expect_equal(nextOr(it, NA), list(a=3, b=5))
  expect_equal(nextOr(it, NA), list(a=1, b=6))
  expect_equal(nextOr(it, NA), list(a=2, b=6))
  expect_equal(nextOr(it, NA), list(a=3, b=6))

  expect_equal(nextOr(it, NA), NA)
})

test_that("igrid constructs the Cartesian product of three named numeric vectors", {
  it <- igrid(a=1:2, b=3:4, c=5:6, rowMajor=FALSE)
  expect_equal(nextOr(it, NA), list(a=1, b=3, c=5))
  expect_equal(nextOr(it, NA), list(a=1, b=3, c=6))
  expect_equal(nextOr(it, NA), list(a=1, b=4, c=5))
  expect_equal(nextOr(it, NA), list(a=1, b=4, c=6))
  expect_equal(nextOr(it, NA), list(a=2, b=3, c=5))
  expect_equal(nextOr(it, NA), list(a=2, b=3, c=6))
  expect_equal(nextOr(it, NA), list(a=2, b=4, c=5))
  expect_equal(nextOr(it, NA), list(a=2, b=4, c=6))

  expect_equal(nextOr(it, NA), NA)
})

test_that("Simplify pastes the index vectors together", {
  it <- igrid(a=1:2, b=3:4, c=5:6, simplify=TRUE)
  expect_equal(nextOr(it, NA), c(a=1, b=3, c=5))
})

test_that("null igrid", {
  it <- igrid(a=numeric(0), b=1:10)
  it(or=NA) %is% NA
  it <- igrid()
  it <- igrid(rowMajor=FALSE)
})

`%do%` <- foreach::`%do%`
foreach <- foreach::foreach
`%:%` <- foreach::`%:%`

test_that("test01", {
    actual <- as.list(igrid(x = 1:3, a = letters[1:2], b = list(1), rowMajor=FALSE))
    expected <- list(list(x = 1L, a = "a", b = 1), list(x = 1L,
        a = "b", b = 1), list(x = 2L, a = "a", b = 1), list(x = 2L,
        a = "b", b = 1), list(x = 3L, a = "a", b = 1), list(x = 3L,
        a = "b", b = 1))
    expect_equal(expected, actual)
})

test_that("test02", {
    it <- igrid(a = LETTERS[1:10], b = list(1), x = 1:3, rowMajor=FALSE)
    results <- foreach(a = LETTERS[1:10], .combine = "c") %:%
        foreach(b = 1, .combine = "c") %:% foreach(x = 1:3, actual = it,
        .combine = "c") %do% identical(list(a = a, b = b, x = x),
        actual)
    expect_true(length(results) == 30)
    expect_true(all(results))
})

test_that("test04", {
  # there is one way to iterate over no variables...
  expect_equal(as.list(igrid()), list(list()))
  # simplifying list() gets NULL
  expect_equal(as.list(igrid(simplify=TRUE)), list(NULL))
  # No ways to iterate over NULL
  expect_equal(as.list(igrid(NULL)), list())
})

test_that("test05", {
    x <- rnorm(100)
    actual <- unlist(as.list(igrid(x)), use.names = FALSE)
    expect_equal(x, actual)
})

test_that("test07", {
    x <- 1:3
    y <- rnorm(5)
    z <- letters[1:4]
    it <- igrid(a = as.list(x), b = y, c = z, rowMajor=FALSE)
    actual <- foreach(d = it, .combine = "rbind") %do% {
        as.data.frame(d, stringsAsFactors = FALSE)
    }
    expected <- expand.grid(c = z, b = y, a = x, stringsAsFactors = FALSE)[3:1]
    expect_equal(expected, actual)
})

test_that("chunked igrid", {

  x <- igrid(a=letters, b=LETTERS, chunkSize=7, simplify=FALSE)
  expected <- expand.grid(a=letters, b=LETTERS,
                          stringsAsFactors=FALSE, KEEP.OUT.ATTRS=FALSE)[1:7,]
  expect_equal(nextOr(x), expected)

  x <- igrid(a=letters, b=LETTERS, chunkSize=7, simplify=TRUE)
  expected <- as.matrix(expand.grid(
    a=letters, b=LETTERS,
    stringsAsFactors=FALSE, KEEP.OUT.ATTRS=FALSE)[1:7,])
  attr(expected, "dimnames") <- list(NULL, c("a", "b"))
  actual <- nextOr(x)
  expect_equal(actual, expected)

})
