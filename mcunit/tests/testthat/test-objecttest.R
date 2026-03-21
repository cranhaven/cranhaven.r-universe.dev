set.seed(12345)

test_that("Methods work when object$test has either one or two arguments", {
  object <- list(genprior=function() rnorm(1),
                gendata=function(theta) NULL,
                stepMCMC=function(theta,data,thinning){
                   rnorm(1)
                }
                )
  control <- list(n=1e2)

  object$test <- function(x) 1
  expect_mcmc(object, control, thinning=1)
  expect_mcmc_reversible(object, control, thinning=1)
  object$test <- function(x,y) 1
  expect_mcmc(object, control, thinning=1)
  expect_mcmc_reversible(object, control, thinning=1)
})

