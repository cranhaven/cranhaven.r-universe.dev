
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tnl.Test

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/ihababusaif/tnl.Test/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ihababusaif/tnl.Test?branch=master)
[![R-CMD-check](https://github.com/ihababusaif/tnl.Test/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ihababusaif/tnl.Test/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/tnl.Test)](https://cran.r-project.org/package=tnl.Test)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- badges: end -->

The goal of tnl.Test is to provide functions to perform the hypothesis
tests for the two sample problem based on order statistics and power
comparisons.

## Installation

You can install the released version of tnl.Test from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tnl.Test")
```

Alternatively, you can install the development version on GitHub using
the devtools package:

``` r
install.packages("devtools") # if you have not installed "devtools" package
devtools::install_github("ihababusaif/tnl.Test")
```

## Details

A non-parametric two-sample test is performed for testing null
hypothesis ${H_0:F=G}$ against the alternative hypothesis
${H_1:F\not=G}$. The assumptions of the ${T_n^{(\ell)}}$ test are that
both samples should come from a continuous distribution and the samples
should have the same sample size.<br /> Missing values are silently
omitted from x and y.<br /> Exact and simulated p-values are available
for the ${T_n^{(\ell)}}$ test. If exact =“NULL” (the default) the
p-value is computed based on exact distribution when the sample size is
less than 11. Otherwise, p-value is computed based on a Monte Carlo
simulation. If exact =“TRUE”, an exact p-value is computed. If
exact=“FALSE”, a Monte Carlo simulation is performed to compute the
p-value. It is recommended to calculate the p-value by a Monte Carlo
simulation (use exact=“FALSE”), as it takes too long to calculate the
exact p-value when the sample size is greater than 10. <br /> The
probability mass function (pmf), cumulative density function (cdf) and
quantile function of ${T_n^{(\ell)}}$ are also available in this
package, and the above-mentioned conditions about exact =“NULL”, exact
=“TRUE” and exact=“FALSE” is also valid for these functions.<br /> Exact
distribution of ${T_n^{(\ell)}}$ test is also computed under Lehman
alternative.<br /> Random number generator of ${T_n^{(\ell)}}$ test
statistic are provided under null hypothesis in the library.

## Examples

`tnl.test` function performs a nonparametric test for two sample test on
vectors of data.

``` r
library(tnl.Test)
require(stats)
 x=rnorm(7,2,0.5)
 y=rnorm(7,0,1)
 tnl.test(x,y,l=2)
#> $statistic
#> [1] 2
#> 
#> $p.value
#> [1] 0.02447552
```

`ptnl` gives the distribution function of ${T_n^{(\ell)}}$ against the
specified quantiles.

``` r
library(tnl.Test)
 ptnl(q=2,n=6,m=9,l=2,exact="NULL")
#> $method
#> [1] "exact"
#> 
#> $cdf
#> [1] 0.01198801
```

`dtnl` gives the density of ${T_n^{(\ell)}}$ against the specified
quantiles.

``` r
library(tnl.Test)
 dtnl(k=3,n=7,m=10,l=2,exact="TRUE")
#> $method
#> [1] "exact"
#> 
#> $pmf
#> [1] 0.02303579
```

`qtnl` gives the quantile function of ${T_n^{(\ell)}}$ against the
specified probabilities.

``` r
library(tnl.Test)
 qtnl(p=c(.1,.3,.5,.8,1),n=8,m=8,l=1,exact="NULL",trial = 100000)
#> $method
#> [1] "exact"
#> 
#> $quantile
#> [1] 2 3 4 6 8
```

`rtnl` generates random values from ${T_n^{(\ell)}}$.

``` r
library(tnl.Test)
 rtnl(N=15,n=7,m=10,l=2)
#>  [1] 7 7 6 7 5 4 7 7 7 5 7 7 7 7 5
```

`tnl_mean` gives an expression for $E({T_n^{(\ell)}})$ under
${H_0:F=G}$.

``` r
library(tnl.Test)
require(base)
 tnl_mean(n.=11,m.=8, l=2)
#> [1] 7.016657
```

`ptnl.lehmann` gives the distribution function of ${T_n^{(\ell)}}$ under
Lehmann alternatives.

``` r
library(tnl.Test)
ptnl.lehmann(q=3, n.=7,m.=7,l = 2, gamma = 1.2)
#> [1] 0.09275172
```

`dtnl.lehmann` gives the density of ${T_n^{(\ell)}}$ under Lehmann
alternatives.

``` r
library(tnl.Test)
 dtnl.lehmann(k=3, n.= 6,m.=8,l = 2, gamma = 0.8)
#> [1] 0.04111771
```

`qtnl.lehmann` returns a quantile function against the specified
probabilities under Lehmann alternatives.

``` r
library(tnl.Test)
qtnl.lehmann(p=.3, n.=4,m.=7, l=1, gamma=0.5)
#> [1] 3
```

`rtnl.lehmann` generates random values from ${T_n^{(\ell)}}$ under
Lehmann alternatives.

``` r
library(tnl.Test)
rtnl.lehmann(N = 15, n. = 7,m.=10, l = 2,gamma=0.5)
#>  [1] 5 6 2 5 7 7 5 7 7 3 7 2 3 2 7
```

## Corresponding Author

Department of Statistics, Faculty of Science, Selcuk University, 42250,
Konya, Turkey <br /> www.researchgate.net/profile/Ihab-Abusaif <br />
Email:<censtat@gmail.com>

## References

Karakaya, K., Sert, S., Abusaif, I., Kuş, C., Ng, H. K. T., & Nagaraja,
H. N. (2023). *A Class of Non-parametric Tests for the Two-Sample
Problem based on Order Statistics and Power Comparisons*. Submitted
paper.<br />

Aliev, F., Özbek, L., Kaya, M. F., Kuş, C., Ng, H. K. T., & Nagaraja, H.
N. (2022). *A nonparametric test for the two-sample problem based on
order statistics.* Communications in Statistics-Theory and Methods,
1-25.
