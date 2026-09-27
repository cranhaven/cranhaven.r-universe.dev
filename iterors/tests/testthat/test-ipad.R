test_that("i_pad functions properly when no fill is necessary", {
  it <- iteror(1:9)
  it_i_pad <- i_pad(it)
  expect_equal(as.list(i_slice(it_i_pad, end=9)), as.list(1:9))
})

test_that("i_pad functions properly with the default fill argument", {
  it <- iteror(1:9)
  it_i_pad <- i_pad(it)
  expect_equal(as.list(i_slice(it_i_pad, end=10)), c(as.list(1:9), NA))
})

test_that("i_pad functions properly with a specified fill argument", {
  it <- iteror(1:9)
  it_i_pad <- i_pad(it, fill=TRUE)
  expect_equal(as.list(i_slice(it_i_pad, end=11)), c(as.list(1:9), as.list(rep(TRUE, 2))))
})
