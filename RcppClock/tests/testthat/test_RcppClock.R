test_that("Testing Rcpp::Clock", {

  fibonacci(n = c(25, 30, 35), reps = 10)
  
  s <- summary(clock)
  expect_equal(all(colnames(s) == c("ticker", "mean", "sd", "min", "max", "neval")), TRUE)
  expect_equal(all(s$neval == 10), TRUE)
  expect_equal(all(!is.na(s)), TRUE)
  
})