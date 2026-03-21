# mcunit

The goal of mcunit is to provide unit tests for MCMC and Monte Carlo
methods. It extends the package testthat.

## Installation

You can install the current  version of mcunit from [bitbucket](https://bitbucket.org) with:

``` r
devtools::install_bitbucket("agandy/mcunit")
```


## Example

This shows how to test if a sampler has a specific mean:


```r
sampler <- function(n) rnorm(n,mean=3.2,1)
expect_mc_iid_mean(sampler,mean=3.2)
expect_mc_iid_mean(sampler,mean=3.5)
#> Error: Test failed with p-value=2.734643e-20 in iteration 1
```

This shows check of a simple MCMC sampler

```r
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
               }
               )
expect_mcmc_reversible(object)
## And now with an error in the sampler:
## sampling until sample is accepted.
object$stepMCMC <- function(theta,data,thinning){
    f <- function(x) prod(dnorm(data,x))*dnorm(x)  
    for (i in 1:thinning){
        repeat{
            thetanew = rnorm(1,mean=theta,sd=1)            
            if (runif(1)<f(thetanew)/f(theta)) break;
        }
        theta <- thetanew
    }
    theta
}
expect_mcmc_reversible(object,control=list(n=1e4))
#> Error: Test failed with p-value=3.586124e-11 in iteration 1
```


