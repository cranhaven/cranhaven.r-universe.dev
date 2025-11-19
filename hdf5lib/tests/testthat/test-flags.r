test_that("c_flags and ld_flags", {
  
  # These functions perform several internal 
  # tests and call `stop()` on invalid states. 
  x <- expect_silent(c_flags())
  y <- expect_silent(ld_flags())
  
  # Expect a string. Not NA or "".
  
  expect_type(x, "character")
  expect_type(y, "character")
  expect_length(x, 1)
  expect_length(y, 1)
  expect_false(is.na(x))
  expect_false(is.na(y))
  expect_true(nzchar(x))
  expect_true(nzchar(y))
  
})
