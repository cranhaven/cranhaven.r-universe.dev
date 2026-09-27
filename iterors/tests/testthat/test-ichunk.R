test_that("i_chunk functions properly without using default value", {
  it <- iteror(letters[1:3])
  it_chunk <- i_chunk(it, size=3)
  expect_equal(nextOr(it_chunk, NULL), as.list(letters[1:3]))
})

test_that("i_chunk does not fill if fill value unspecified", {
  it <- iteror(letters[1:3])
  it_chunk <- i_chunk(it, size=2)
  expect_equal(nextOr(it_chunk, NULL), as.list(letters[1:2]))
  expect_equal(nextOr(it_chunk, NULL), list(letters[3]))
})

test_that("i_chunk functions properly when fill value is specified", {
  it <- iteror(letters[1:3])
  it_chunk <- i_chunk(it, size=2, fill='weeeeee')
  expect_equal(nextOr(it_chunk, NULL), as.list(letters[1:2]))
  expect_equal(nextOr(it_chunk, NULL), list(letters[3], 'weeeeee'))
})

test_that("i_chunk rejects non-positive or non-numeric n", {
  it <- iteror(letters)
  error_msg <- "size' must be a positive number of length 1"
  expect_error(i_chunk(it, size=-1), error_msg)
  expect_error(i_chunk(it, "1"), error_msg)
})

test_that("non-integer size", {
  it <- i_chunk(letters, 3.5, "character")
  pasta <- i_apply(it, paste0, collapse="")
  expect_equal(as.character(pasta),
               c("abcd", "efg", "hijk", "lmn", "opqr", "stu", "vwxy", "z"))
})

test_that("test01", {
    x <- 1:10
    actual <- as.list(i_chunk(x, 3))
    expected <- list(as.list(1:3), as.list(4:6), as.list(7:9),
        as.list(10))
    expect_equal(expected, actual)
})

test_that("test02", {
    n <- 13
    chunkSize <- 7
    x <- rnorm(n * chunkSize)
    actual <- as.list(i_chunk(x, chunkSize))
    expect_true(length(actual) == n)
    for (chunk in actual) {
        expect_true(length(chunk) == chunkSize)
    }
    y <- unlist(actual)
    expect_equal(y, x)
})
