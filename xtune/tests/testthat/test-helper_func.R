library(numDeriv)

set.seed(1234)
test_that("gradient function match with numeric gradient", {
  X <- matrix(runif(100), nrow = 20)
  Y <- 1:20
  Z <- matrix(runif(10), nrow = 5)
  theta = as.vector(rnorm(ncol(X),0,1))
  delta = as.vector(rnorm(ncol(X),0,1))
  expect_equal(grad(likelihood.alpha.theta.xtune,rep(0,ncol(Z)),Z=Z,theta=theta,delta= delta, c = 0.5),
               as.vector(likelihood.alpha.theta.gradient.xtune(Z = Z, c = 0.5, alpha = rep(0,ncol(Z)),theta = theta,delta = delta)))

  X <- matrix(runif(100), nrow = 20)
  Y <- as.factor(sample(1:3, 20, replace = T))
  Z <- matrix(runif(10), nrow = 5)
  theta = rbind(as.vector(rnorm(ncol(X),0,1)),as.vector(rnorm(ncol(X),0,1)),as.vector(rnorm(ncol(X),0,1)))
  delta = list(as.vector(rnorm(ncol(X),0,1)),as.vector(rnorm(ncol(X),0,1)),as.vector(rnorm(ncol(X),0,1)))
  expect_equal(grad(likelihood.alpha.theta.mxtune,rep(0,ncol(Z)),Z=Z,theta=theta,delta= delta, c = 0.5, k = 3),
               as.vector(likelihood.alpha.theta.gradient.mxtune(Z = Z, c = 0.5, alpha = rep(0,ncol(Z)),theta = theta,delta = delta, k = 3)))
}
)

