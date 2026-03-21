set.seed(12345)

test_that("MH-Normaldistr",{
    object <- list(genprior=function() rnorm(1),
                   gendata=function(theta) rnorm(5,theta),
                   stepMCMC=function(theta,data,thinning){
                       f <- function(x) prod(dnorm(data,x))*dnorm(x)  
                       for (i in 1:thinning){
                           thetanew = rnorm(1,mean=theta,sd=1)
                           if (runif(1)<f(thetanew)/f(theta))
                               theta <- thetanew
                       }
                       theta
                   } )
    expect_mcmc_reversible(object)
    expect_mcmc(object)
})


test_that("MH-Normaldistr-mean-incorrect",{
    object <- list(genprior=function() rnorm(1),
                   gendata=function(theta) rnorm(5,theta),
                   stepMCMC=function(theta,data,thinning){
                       f <- function(x) prod(dnorm(data,mean=x+1))*dnorm(x)  
                       for (i in 1:thinning){
                           thetanew = rnorm(1,mean=theta,sd=1)
                           if (runif(1)<f(thetanew)/f(theta))
                               theta <- thetanew
                       }
                       theta
                   } )
    control <- list(n=4e3)
    expect_error(expect_mcmc_reversible(object,control=control))
    control <- list(n=1e3)
    expect_error(expect_mcmc(object,control=control))
})

test_that("MH-Normaldistr-sd-incorrect",{
    skip_on_cran()
    object <- list(genprior=function() rnorm(1),
                   gendata=function(theta) rnorm(5,theta),
                   stepMCMC=function(theta,data,thinning){
                       f <- function(x) prod(dnorm(data,mean=x,sd=sdwrong))*dnorm(x)  
                       for (i in 1:thinning){
                           thetanew = rnorm(1,mean=theta,sd=1)
                           if (runif(1)<f(thetanew)/f(theta))
                               theta <- thetanew
                       }
                       theta
                   } )


    sdwrong <- 1.5
    control <- list(n=4e3)
    expect_error(expect_mcmc_reversible(object,control=control))
    control <- list(n=5e4,level=1e-3)
    expect_error(expect_mcmc(object,control=control))
    sdwrong <- 0.5
    control <- list(n=4e3)
    expect_error(expect_mcmc_reversible(object,control=control))
    control <- list(n=5e4,level=1e-3)
    expect_error(expect_mcmc(object,control=control))

    sdwrong <- 1 ##now with correct sd
    control <- list(n=4e3)
    expect_mcmc_reversible(object,control=control)
    control <- list(n=5e4,level=1e-3)
    expect_mcmc(object,control=control)

    
})

