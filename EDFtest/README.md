# EDFtest
This package contains software for the calculation of goodness-of-fit
test statistics and their P-values. The three statistics computed are the
Empirical Distribution function statistics called Cramér-von Mises, Anderson-Darling,
and Watson statistic.  

The statistics and their P-values can be used to assess an assumed distribution. In the simplest situation
you have an i.i.d. sample from some distribution F and want to test the hypothesis that the sample is drawn from
a distribution F which belongs to a specified parametric family of distributions against the alternative that 
F is not equal to any member of that parametric family. The following families are available:
Uniform(min, max),
Normal(location, scale),
Gamma(shape, scale),
Logistic(location, scale),
Laplace(location, scale),
Weibull(shape, scale),
Extreme Value(shape, scale), and
Exponential(scale).

This package also contains function `gof.sandwich` which performs Goodness-of-Fit tests for general distributions
using Sandwich estimation of covariance function. This function tests the hypothesis that data y come from 
distribution `Fdist` with unknown parameter values theta. Estimates of theta must be provided.
It uses a large sample approximation to the limit distribution based on the use of the score function components
to estimate the Fisher information and the limiting covariance function of the empirical process.


Authors:

*   [Li Yao](https://github.com/LiYao-sfu),
    <yaoliy@sfu.ca> (Maintainer)
*   [Richard Lockhart](http://www.sfu.ca/~lockhart/),
    <lockhart@sfu.ca>

Papers:

*   Stephens, M.A. (1974). [EDF Statistics for Goodness of Fit and Some Comparisons.](https://doi.org/10.2307/2286009) *Journal of the American Statistical Association*, Vol. 69, 730-737.
*   Stephens, M.A. (1976). [Asymptotic results for goodness-of-fit statistics
     with unknown parameters.](https://scholar.google.com/scholar_url?url=https://projecteuclid.org/journals/annals-of-statistics/volume-4/issue-2/Asymptotic-Results-for-Goodness-of-Fit-Statistics-with-Unknown-Parameters/10.1214/aos/1176343411.pdf&hl=en&sa=T&oi=ucasa&ct=ufr&ei=Wmd0YePQH_iM6rQPgq2vuAg&scisig=AAGBfm09GbS_cTMrgCfM7KEYz6yjOKMUfQ) *Annals of Statistics*, Vol. 4, 357-369.


## Installation
There are several ways you can install GitHub packages into R. For example,
You can install our package by using `devtools`. You need to install `devtools` package first if you have not.


Step 1: Install the `devtools` package
```R
install.packages("devtools")
```

Step 2: Install our `EDFtest` package and attach it
```R
library(devtools)
install_github("LiYao-sfu/EDFtest")
library("EDFtest")
```

## Troubleshooting
This package is still under development. EDF test for regression models and discrete distributions 
will be available for the future releases.

If you encounter a clear bug, You could create an issue on GitHub. For other questions, please contact Li Yao by <yaoliy@sfu.ca>.
