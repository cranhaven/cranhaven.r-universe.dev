test_that("conversions", {
  expect_equal(as.character(iteror(letters)), letters)
  expect_equal(as.numeric(icount(10)), seq_len(10))
  expect_equal(as.double(icount(10)), seq_len(10))
  expect_equal(as.list(icount(10)), as.list(seq_len(10)))
  expect_equal(as.logical(iteror(c(TRUE, FALSE))),
               c(TRUE, FALSE))
})
