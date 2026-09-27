# test coverage doesn't hit the actual templated functions so here's a fake
test_that("template iteror constructor", {

  ivector <- count_template(
    input = alist(obj=),
    preamble = alist(
      count <- length(obj)),
    output = function(ix) substitute(obj[[ix]]), # unboxing!!!,
    output_chunk = function(ix, len) substitute(obj[ix + seq_len(len)])
  )

  expect_equal(as.numeric(ivector(1:10)), 1:10)
  expect_equal(c(recursive=TRUE,
                 as.list(ivector(1:10, chunkSize=3))),
               1:10)
  expect_equal(c(recursive=TRUE,
                 take(ivector(1:10, chunkSize=3, recycle=TRUE), 6)),
               c(1:10, 1:6))
  expect_equal(c(recursive=TRUE,
                 as.list(ivector(1:10, chunks=3))),
               1:10)
  expect_equal(c(recursive=TRUE,
                 take(ivector(1:10, chunks=3, recycle=TRUE), 6)),
               c(1:10, 1:10))

  integers <- count_template(
    input = alist(count=Inf),
    output = function(ix) substitute(ix),
    output_chunk = function(ix, len) substitute(ix + seq_len(len))
  )

  expect_equal(c(recursive=TRUE,
                 take(integers(chunkSize=3, recycle=TRUE), 6)),
               c(1:18))
  expect_equal(c(recursive=TRUE,
                 take(integers(10, recycle=TRUE), 11)),
               c(1:10, 1))
  expect_equal(c(recursive=TRUE,
                 take(integers(10, chunks=3, recycle=TRUE), 4)),
               c(1:10, 1:4))

})
