test_that("Lasso distribution functions return expected types", {
  a <- 2
  b <- 1
  c <- 3
  logarithm <- FALSE
  
  x <- seq(-2, 2, length.out = 5)
  p <- c(0.25, 0.5, 0.75)
  q <- c(-1, 0, 1)
  n <- 10
  d <- 1
  
  expect_type(zlasso(a, b, c, logarithm), "double")
  expect_length(zlasso(a, b, c, logarithm), 1)
  
  expect_type(dlasso(x, a, b, c, logarithm), "double")
  expect_length(dlasso(x, a, b, c, logarithm), length(x))
  
  expect_type(plasso(q, a, b, c), "double")
  expect_length(plasso(q, a, b, c), length(q))
  
  expect_type(qlasso(p, a, b, c), "double")
  expect_length(qlasso(p, a, b, c), length(p))
  
  expect_type(rlasso(n, a, b, c), "double")
  expect_length(rlasso(n, a, b, c), n)
  
  expect_type(elasso(a, b, c), "double")
  expect_length(elasso(a, b, c), 1)
  
  expect_type(vlasso(a, b, c), "double")
  expect_length(vlasso(a, b, c), 1)
  
  expect_type(mlasso(a, b, c), "double")
  expect_length(mlasso(a, b, c), 1)
  
  expect_type(MillsRatio(d), "double")
  expect_length(MillsRatio(d), 1)
})
