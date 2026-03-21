set.seed(12345)

test_that("Normaldistr",{
    object <- list(genprior=function() rnorm(1),
                   gendata=function(theta) NULL,
                   stepMCMC=function(theta,data,thinning){
                       rnorm(1)
                   }
                   )
    control <- list(n=1e3, nsteps=1, debug=0)
    expect_mcmc(object,control)
    control$nsteps <- 10
    expect_mcmc_reversible(object,control)
})

test_that("Normaldistr Shift Mean",{
    object <- list(genprior=function() rnorm(1),
                   gendata=function(theta) NULL,
                   stepMCMC=function(theta,data,thinning){
                       rnorm(1,mean=0.3)
                   }
                   )
    expect_error(expect_mcmc(object,control))
    expect_error(expect_mcmc_reversible(object))
})


test_that("No Automatic Choice of Lag possible",{
    object <- list(genprior=function() rnorm(1),
                   gendata=function(theta) NULL,
                   stepMCMC=function(theta,data,thinning){
                       theta
                   }
                   )
    control <- list(n=1e3, nsteps=1, debug=0)
    expect_error(expect_mcmc(object,control))
    control$nsteps <- 10
    expect_error(expect_mcmc_reversible(object,control))
})


test_that("Normaldistr_2D",{
    object <- list(genprior=function() rnorm(2),
                   gendata=function(theta) NULL,
                   stepMCMC=function(theta,data,thinning){
                       rnorm(2)
                   }
                   )
    control <- list(n=1e3, nsteps=1)
    expect_mcmc(object,control)
    control$nsteps <- 10
    expect_mcmc_reversible(object,control)

    object$test <- function(x) x[1]
    control$nsteps <- 1
    expect_mcmc(object,control)
    control$nsteps <- 10
    expect_mcmc_reversible(object,control)


    object$test <- function(x) c(x[1],x[2],sum(x))
    control$nsteps <- 1
    expect_mcmc(object,control)
    control$nsteps <- 10
    expect_mcmc_reversible(object,control)
})



test_that("Normaldistr_2D_wrongmodel",{
    object <- list(genprior=function() rnorm(2),
                   gendata=function(theta) NULL,
                   stepMCMC=function(theta,data,thinning){
                       rnorm(2,mean=c(0,1))
                   }
                   )
    control <- list(n=1e3, nsteps=1)
    expect_error(expect_mcmc(object,control))
    control$nsteps <- 10
    expect_error(expect_mcmc_reversible(object,control))

    object$test <- function(x) x[1]
    control$nsteps <- 1
    expect_mcmc(object,control)
    control$nsteps <- 10
    expect_mcmc_reversible(object,control)

    object$test <- function(x) x[2]
    control <- list(n=1e3, nsteps=1)
    expect_error(expect_mcmc(object,control))
    control$nsteps <- 10
    expect_error(expect_mcmc_reversible(object,control))
})


