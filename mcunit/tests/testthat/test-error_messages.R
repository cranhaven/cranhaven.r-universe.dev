set.seed(12345)

test_that("correct arguments", {
  expect_error(expect_mcmc_reversible(5,3))
  expect_error(expect_mcmc(5,3))
})


test_that("correct arguments expect_mcmcreversible", {
 object <- list(genprior=function() rnorm(1),
                gendata=function(theta) NULL,
                stepMCMC=function(theta,data,thinning){
                   rnorm(1)
                }
                )
 control <- list(n=1e3)

 expect_error(expect_mcmc_reversible(object,control,nsteps=1)) ##nsteps must be at least 2
 expect_mcmc_reversible(object,control,nsteps=2) ##nsteps must be at least 2
 expect_mcmc_reversible(object,control) ##default should work
})

