test_that("ilength functions properly with a vector", {
  expect_equal(count(1:5), 5)
})

test_that("ilength functions properly with an iteror", {
  it <- iteror(1:5)
  expect_equal(count(it), 5)
})

test_that("ilength functions properly with a chained iteror", {
  it <- i_chain(1:3, 4:5, 6)
  expect_equal(count(it), 6)
})

test_that("ilength functions properly with a chained iteror of mixed types", {
  it <- i_chain(1:3, levels(iris$Species))
  expect_equal(count(it), 6)
})
