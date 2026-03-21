set.seed(12345)

test_that("Bernoullidistr",{
    object <- list(genprior=function() rbinom(1,size=2,prob=p),
                   gendata=function(theta) NULL,
                   stepMCMC=function(theta,data,thinning){
                       rbinom(1,size=2,prob=p)
                   }
                   )
    
    p <- 0.5
    control <- list(n=1e3, nsteps=1, debug=0)
    expect_mcmc(object,control)
    control$nsteps <- 10
    expect_mcmc_reversible(object,control)

    p <- 0.1
    control <- list(n=1e3, nsteps=1, debug=0)
    expect_mcmc(object,control)
    control$nsteps <- 10
    expect_mcmc_reversible(object,control)

})
