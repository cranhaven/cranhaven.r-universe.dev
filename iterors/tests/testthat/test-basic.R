nextElem <- iterators::nextElem
`%is%` <- expect_equal


test_that("test vector iterator creation", {
  expect_silent(x <- iteror(1:10))
})

test_that("test hasNext, nextElem", {
  x <- iteror(1:10)
  expect_equal(nextElem(x), 1)
  for(i in 1:9) nextElem(x)
  expect_error(nextElem(x))
})

test_that("test matrix iterator creation", {
  x <- matrix(1:10,ncol=2)
  expect_silent(i <- iteror(x, by='cell'))
})

test_that("test hasNext, nextElem", {
  x <- matrix(1:10,ncol=2)
  # by cell
  y <- iteror(x, by='cell')
  expect_equal(nextElem(y), 1)
  for(i in 1:9) nextElem(y)
  expect_error(nextElem(y))

  # by col
  y <- iteror(x,by='column')
  expect_equal(nextElem(y), matrix(1:5, ncol=1))
  nextElem(y)
  expect_error(nextElem(y))

  # by col, dropping
  y <- iteror(x, by='column', drop=TRUE)
  expect_equal(nextElem(y), 1:5)
  nextElem(y)
  expect_error(nextElem(y))

  # by row
  y <- iteror(x,by='row')
  expect_equal(nextElem(y), matrix(c(1,6),nrow=1))
  for(i in 1:4) nextElem(y)
  expect_error(nextElem(y))
})


test_that("test data frame iterator creation", {
  x <- data.frame(1:10, 11:20)
  expect_silent(y <- iteror(x))
})

test_that("test hasNext, nextElem", {
  x <- data.frame(1:10, 11:20)
  # by row
  y <- iteror(x, by='row')
  expect_equal(nextElem(y), x[1,])
  for(i in 1:9) nextElem(y)
  expect_error(nextElem(y))

  # by col
  y <- iteror(x, by='column')
  expect_equal(nextElem(y), x[,1])
  nextElem(y)
  expect_error(nextElem(y))
})

test_that("empty data frames", {

  x <- as.data.frame(list(a=c(1), b=c(2)))
  emp <- x[c(),]

  it <- iteror(emp, by="row")
  nextOr(it, NA) %is% NA

  it <- iteror(emp, by="col")
  nextOr(it, NA) %is% numeric(0)
  nextOr(it, NA) %is% numeric(0)
  nextOr(it, NA) %is% NA

  emp2 <- data.frame()
  it <- iteror(emp2, by="col")
  nextOr(it, NA) %is% NA

})

# test function iterator creation
test_that("iteror from functions", {
  normalFunc <- (
    function() {i <- 0; function(or) if (i >= 10) or else (i <<- i + 1)})
  throwingFunc <- (
    function() {i <- 0; function() if (i >= 10) stop('the end') else (i <<- i + 1)})
  sentinelFunc <- (
    function() {i <- 0; function() {if (i >= 10) 'the end' else (i <<- i + 1)}})
  countFunc <- (
    function() {i <- 0; function() (i <<- i + 1)})

  ia <- iteror(normalFunc())
  ib <- iteror(throwingFunc(), catch='the end')
  ic <- iteror(sentinelFunc(), sentinel='the end')
  id <- iteror(countFunc(), count=10)

  expect_length(as.list(i_zip(ia, ib, ic, id)), 10)

  ia(NULL) %is% NULL
  ib(NULL) %is% NULL
  ic(NULL) %is% NULL

  if(FALSE) {
    y <- iteror(needArgFunc, catch='too high')
    expect_equal(nextElem(y), 1)
    for (i in 1:99) nextElem(y)
    expect_error(nextElem(y))
  }

})

# itertools2's internal function `stop_iteration` has a bug
# because it does this:
#     inherits(object, "try-error") && object == "Error : StopIteration\n"
# which fails because in practice the error message stop("StopIteration")
# produces is more like
#     "Error in obj(or = stop(\"StopIteration\"), ...) : StopIteration\n"
# So backfill nextElem.iteror has to use call.=FALSE
test_that("just the right message", {
  result <- try(nextElem(iteror(c())), silent=TRUE)
  expect_true(
    inherits(result, "try-error") && result == "Error : StopIteration\n")
})
