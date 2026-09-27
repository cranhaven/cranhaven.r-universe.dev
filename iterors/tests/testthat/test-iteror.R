`%is%` <- expect_equal

test_that("hasNext", {
  i <- ihasNext(iseq(1, 10))
  for (j in 1:10) {
    hasNext(i) %is% TRUE
    iterators::nextElem(i) %is% j
 }
  hasNext(i) %is% FALSE
  expect_error(iterators::nextElem(i), "StopIteration")
})

test_that("compatibility with iterators", {
    it <- iterators::iter(seq(40, 120, length.out=3))

    nextOr(it) %is% 40
    it2 <- iteror(it)
    expect_true("iteror" %in% class(it2))
    nextOr(it2) %is% 80
    iterators::nextElem(it2) %is% 120
    expect_error(iterators::nextElem(it2), "StopIteration")
})

