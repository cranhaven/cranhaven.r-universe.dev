set.seed(123456)

test_that("test for coef",{
  X <- matrix(runif(30), ncol = 3)
  Y <- 1:10
  U <- as.matrix(rnorm(10))
  Z <- matrix(runif(6), nrow = 3)
  xtune_fitted <- xtune(X,Y,Z, U, family = "linear")
  expect_equal(coef_xtune(xtune_fitted),xtune_fitted$beta.est)
})

