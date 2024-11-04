test_that("x100 is correct size", {
  #data(x100)
  expect_equal(nrow(x100), 100)
  expect_equal(ncol(x100), 6)
})
