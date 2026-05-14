## varjmcm

The goal of varjmcm is to equip 'jmcm' package with estimations of the covariance of estimated parameters. Two methods are provided. The first method is bootstrap based. The second method is to use the inverse of estimated Fisher's information matrix as the estimated covariance.

The bootstrap method may need large number of replications and thus may be very time consuming, especially for the HPC model. The explicit formula in the second method is asymptotically correct, and thus is valid only when the sample size is large. When the sample size is large, the second method is a better choice. Results from these two methods are close to each other if both the number of replications and the sample size are large, otherwise, they may be very different.

The current version of the 'jmcm' package is 0.1.8.0. The 'varjmcm' package is based on this version of 'jmcm'. For further versions of 'jmcm', updates in 'varjmcm' may be needed.

## Example

This is a basic example.

``` r
## cattleA <- cattle[cattle$group=='A', ]
## fit.mcd <- jmcm(weight|id|I(ceiling(day/14+1))~1|1,
                data = cattleA,
                cov.method = "mcd",
                triple = c(8,4,3))
## covjmcm(fit.mcd)
## bootcovjmcm(fit.mcd, 
               mydata = cattleA, 
               numboot = 100)
```
