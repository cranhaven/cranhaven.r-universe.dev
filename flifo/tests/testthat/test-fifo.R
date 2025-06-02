context("Test FIFO stacks")

test_that("a new FIFO stack is empty, is a 'stack' object, 
          and is a 'fifo' object", {
  s <- fifo(max_length = 2)
  expect_true(is.empty(s))
  expect_true(is.stack(s))
  expect_true(is.fifo(s))
  xcopy <- x <- stats::runif(1)
  push(s, x)
  push(s, 7)
  expect_error(push(s, 1)) # max length is reach, stack is full
  expect_identical(xcopy, pop(s))
  expect_identical(7, pop(s))
})
