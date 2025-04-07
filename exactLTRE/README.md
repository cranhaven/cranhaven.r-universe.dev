
<!-- README.md is generated from README.Rmd. Please edit that file -->

# exactLTRE

<!-- badges: start -->

[![R-CMD-check](https://github.com/chrissy3815/exactLTRE/workflows/R-CMD-check/badge.svg)](https://github.com/chrissy3815/exactLTRE/actions)
<!-- badges: end -->

The goal of exactLTRE is to provide a set of friendly tools to increase
use of exact LTRE methods. Life Table Response Experiments are a
valuable tool for advancing our knowledge about how stage-specific vital
rates influence population dynamics. The results of LTREs also inform
conservation goals, by identifying the life stage and vital rate (for
example, juvenile survival or adult reproductive success) that had the
largest impact on population growth rate in past observations or
following a management intervention.

The classical methods of LTRE, presented in the Caswell (2001) textbook,
use Taylor expansion approximations to calculate the contributions of
vital rates. These classical methods also do not include many
interactions terms which may be important. Exact LTRE provides an exact
calculation of each contribution term, including interactions terms, by
directly calculating the impact on population growth rate of differences
or variation in vital rates.

This package includes functions for the classical methods as well as the
fANOVA-based exact methods that we introduce. This will allow
researchers to use both the methods that are standard in the literature
up until now (2021), and the new methods that we encourage as a new
standard.

## Installation

You can install the development version of exactLTRE from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("chrissy3815/exactLTRE")
# if that doesn't work, try:
devtools::install_github("chrissy3815/exactLTRE", ref="main")
```

## Fixed Design LTRE

A fixed design LTRE decomposes the difference in population growth rate
(“lambda,” i.e. the leading eigenvalue of the population projection
matrix) into the contributions from differences in the vital rates, and
their interactions.

A fixed design LTRE compares exactly two matrices. This can be done
either as a symmetric or directional analysis, and it should match the
experimental design for the data collection.

``` r
library(exactLTRE)
## basic example code
A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
# contributions to the difference in lambda for a directional design
cont_directional<- exactLTRE(list(A1,A2), method='fixed', fixed.directional=TRUE) 
# contributions to the difference in lambda for a symmetric design
cont_symmetric<- exactLTRE(list(A1,A2), method='fixed', fixed.directional=FALSE)
```

## Random Design LTRE

A random design LTRE decomposes the variance in population growth rate
(“lambda,” i.e. the leading eigenvalue of the population projection
matrix) into the contributions from (co)variances of the vital rates and
their interactions.

A random design LTRE can be used on an unordered list of two or more
matrices.

``` r
library(exactLTRE)
## basic example code
A1<- matrix(data=c(0,0.8,0, 0,0,0.7, 5,0,0.2), nrow=3, ncol=3)
A2<- matrix(data=c(0,0.9,0, 0,0,0.5, 4,0,0.3), nrow=3, ncol=3)
A3<- matrix(data=c(0,0.4,0, 0,0,0.6, 6,0,0.25), nrow=3, ncol=3)
# contributions to the variance of lambda
cont_var<- exactLTRE(list(A1,A2,A3), method='random')
```

<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->
<!-- ```{r cars} -->
<!-- summary(cars) -->
<!-- ``` -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>. -->
<!-- You can also embed plots, for example: -->
<!-- ```{r pressure, echo = FALSE} -->
<!-- plot(pressure) -->
<!-- ``` -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
