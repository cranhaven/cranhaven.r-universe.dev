# Tests for internal (non-exported functions)

test_that("internal commesurability test of vector lenghts errs", {
  expect_true( commensurable(as.integer(c(  1,   1,  1        ))))
  expect_true( commensurable(as.integer(c(  1,   1,  10,  1   ))))
  expect_true( commensurable(as.integer(c(  1,   1,  12, 12, 1))))
  expect_true( commensurable(as.integer(c(100, 100, 100       ))))
  expect_false(commensurable(as.integer(c(  1,   1,   2,  4   ))))
  expect_false(commensurable(as.integer(c( 20,  40            ))))
})
