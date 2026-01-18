
ESTER: Efficient Sequential Testing with Evidence Ratios
========================================================

[![CRAN status](https://www.r-pkg.org/badges/version/ESTER)](https://cran.r-project.org/package=ESTER) [![Build Status](https://travis-ci.org/lnalborczyk/ESTER.svg?branch=master)](https://travis-ci.org/lnalborczyk/ESTER)

The `ESTER` package implements sequential testing based on evidence ratios computed from the Akaike weights of a set of models. These weights are being computed using either the Akaike Information Criterion (AIC) or the Bayesian Information Criterion (BIC).

Installation
------------

To install the latest version from CRAN you can use

``` r
install.packages("ESTER")
```

Or the development version can be installed with

``` r
if (!require("devtools") ) install.packages("devtools")
devtools::install_github("lnalborczyk/ESTER", dependencies = TRUE)
```

Different questions
-------------------

1.  **Simulation**. Given an expected effect size and a given sample size, what evolution of evidence ratios should I reasonnably expect ?

2.  **Observed data**. When to stop recruiting participants ?

Simulation
----------

The `simER` function runs a simulated study in which we compare two independant groups, for various effect size (`cohensd`) and sample size (`nmax`). The `nmin` argument serves to specify from which participant we want to start doing sequential testing (we usually recommend to avoid `nmin` &lt; 10). We can define a `boundary` at which we would like to stop the sequential testing, as well as how many simulations we want to evaluate (`nsims`).

``` r
library(ESTER)
simER(cohensd = 0.8, nmin = 20, nmax = 100, boundary = 10, nsims = 100, ic = bic)
```

Observed data
-------------

On the other hand (and perhaps more interestingly), `ESTER` can be used to do sequential testing on your own data. You can study the evolution of sequential ERs using the `seqER` function.

``` r
data(mtcars)
mod1 <- lm(mpg ~ cyl, mtcars)
mod2 <- lm(mpg ~ cyl + disp, mtcars)
seqER(ic = bic, mod1, mod2, nmin = 10)
```

In addition, `seqER` allows you to study the behavior of sequential ERs computed on your own data, along with sequential ERs computed on permutation samples. This feature might be useful to study to what extent the evolution of evidence ratios you observed on the original sample is dependent to the order of the observations.

``` r
seqER(ic = bic, mod1, mod2, nmin = 10, nsims = 10)
```

More detailed information can be found in the main vignette, available online [here](https://rawgit.com/lnalborczyk/ESTER/master/inst/doc/ESTER.html), or by typing `vignette("ESTER")` in the console.
