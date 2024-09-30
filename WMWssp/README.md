# WMWssp 0.4.0

[![CRANstatus](https://www.r-pkg.org/badges/version/WMWssp)](https://cran.r-project.org/package=WMWssp)
[![](https://cranlogs.r-pkg.org/badges/WMWssp)](https://cran.r-project.org/package=WMWssp)
[![Travis-CI Build Status](https://travis-ci.org/happma/WMWssp.svg?branch=master)](https://travis-ci.org/happma/WMWssp)
[![Build status](https://ci.appveyor.com/api/projects/status/1o3r47cxb7oejhpl?svg=true)](https://ci.appveyor.com/project/happma/wmwssp)
[![codecov](https://codecov.io/gh/happma/WMWssp/branch/master/graph/badge.svg)](https://codecov.io/gh/happma/WMWssp)
[![DOI](https://img.shields.io/badge/DOI-10.1002%2Fsim.7983-blue.svg)](http://dx.doi.org/10.1002/sim.7983)

Calculates the minimal sample size for the Wilcoxon-Mann-Whitney test that is needed for a given power and two sided type I error rate. The method works for metric data with and without ties, count data, ordered categorical data, and even dichotomous data. But data is needed for the reference group to generate synthetic data for the treatment group based on a relevant effect.
For details, see for example [1] or [2].

To install the current development version:

``` r
## install devtools package if it's not already
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
# install package
devtools::install_github("happma/WMWssp")
library(WMWssp)
```

To calculate the sample size we need prior information from one group. Let us call this group reference group. Based on this reference group, we can create artifical data according to an interpretable effect. Note that we have to specify, how many subjects should be assigned to the first and how many to the second group.
``` r
# Prior information for the reference group
x <- c(315,375,356,374,412,418,445,403,431,410,391,475,379)
# generate data for treatment group based on a shift effect
y <- x - 20

# calculate sample size
ssp <- WMWssp(x, y, alpha = 0.05, power = 0.8, t = 1/2)
summary(ssp)
```
It is also possible to vary the allocation rate to even further reduce the sample size. But for almost all situations, a balanced design will be optimal or close to optimal, see [2] or [3].
``` r
# calculate optimal allocation rate t
ssp <- WMWssp_minimize(x, y, alpha = 0.05, power = 0.8)
summary(ssp)
```

# References

[1] Brunner, E., Bathke A. C. and Konietschke, F: Rank- and Pseudo-Rank Procedures in Factorial Designs - Using R and SAS, Springer Verlag, to appear,

[2] <a href=" https://doi.org/10.1002/sim.7983">Happ, M., Bathke, A. C., & Brunner, E. (2019). Optimal sample size planning for the Wilcoxon‐Mann‐Whitney test. Statistics in medicine, 38(3), 363-375.</a>

[3] Bürkner, P‐C, Doebler, P, Holling, H. Optimal design of the Wilcoxon–Mann–Whitney‐test. Biom J. 2017; 59( 1): 25‐ 40. 
