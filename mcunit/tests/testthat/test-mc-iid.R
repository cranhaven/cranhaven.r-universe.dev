set.seed(12345)

test_that("Normaldistr",{

    print(runif(1))
    sampler <- function(n) rnorm(n)
    expect_mc_iid_ks(sampler, pnorm)

    expect_error(expect_mc_iid_ks(sampler,function(x) pnorm(x,mean=0.5)))
    expect_error(expect_mc_iid_ks(sampler,function(x) pnorm(x,mean=0.2)))
    expect_error(expect_mc_iid_ks(sampler,function(x) pnorm(x,sd=1.3)))
    expect_error(expect_mc_iid_ks(sampler,function(x) pnorm(x,sd=0.6)))
})

test_that("Gammadistr",{
    control=list(n=1e4)

    sampler <- function(n) rgamma(n,shape=2)
    expect_mc_iid_ks(sampler, function(x) pgamma(x,shape=2),control=control)
    expect_error(expect_mc_iid_ks(sampler, function(x) pgamma(x,shape=1.95),control=control))

})

test_that("Binomial",{
    sampler <- function(n) rbinom(n,prob=0.7,size=5)
    control=list(n=1e4)
    expect_mc_iid_chisq(sampler, dbinom(0:5,prob=0.7,size=5),control=control)
    expect_error(expect_mc_iid_chisq(sampler, dbinom(0:5,prob=0.5,size=10),control=control))
    expect_error(    expect_mc_iid_chisq(sampler, dbinom(0:5,prob=0.65,size=10),control=control))
    expect_error(    expect_mc_iid_chisq(sampler, dbinom(0:5,prob=0.69,size=10),control=control))
})


test_that("Mean",{
    sampler <- function(n) rnorm(n,mean=0,1)
    
    expect_mc_iid_mean(sampler, m=0)
    expect_error(expect_mc_iid_mean(sampler, mean=0.2))
    expect_error(expect_mc_iid_mean(sampler, mean=-0.2))
})
