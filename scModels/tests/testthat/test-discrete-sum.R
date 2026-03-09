
test_that("Discrete probabilities sum to unity", {
  expect_equal(sum(dpb(0:100, 5, 3, 20)), 1)
  expect_equal(sum(dpb(x = 0:4000, alpha = 1.05, beta = 5.5, c = 709)), 1)
  expect_equal(sum(dpb(x = 0:4000, alpha = 1.05, beta = 5.5, c = 2009)), 1)
  expect_equal(sum(dpb(x = 0:4000, alpha = 1.05, beta = 5.5, c = 4009)), 1)
})
