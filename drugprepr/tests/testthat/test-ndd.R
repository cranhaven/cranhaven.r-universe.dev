test_that("compute_ndd produces expected output", {
  output1 <- compute_ndd(dataset1, min, min)
  mean <- mean(output1$ndd, na.rm = TRUE)
  expect_equal(mean, 3.4285714286)
  expect_equal(nrow(output1), 18L)

  output2 <- compute_ndd(dataset1, min, max)
  mean <- mean(output2$ndd, na.rm = TRUE)
  expect_equal(mean, 3.4285714286)

  output3 <- compute_ndd(dataset1, max, max)
  mean <- mean(output3$ndd, na.rm = TRUE)
  expect_equal(mean, 6.8571428571)
})
