
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/gorica)](https://cran.r-project.org/package=gorica)
[![downloads](https://cranlogs.r-pkg.org/badges/gorica)](https://cran.r-project.org/package=gorica)
[![R-CMD-check](https://github.com/cjvanlissa/gorica/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cjvanlissa/gorica/actions/workflows/R-CMD-check.yaml)
<!-- [![DOI](http://joss.theoj.org/papers/10.21105/joss.00978/status.svg)](10.1111/bmsp.12110)-->

# GORICA: Evaluation of Inequality Constrained Hypotheses Using Generalized AIC

Implements the generalized order-restricted information criterion
approximation (GORICA). The GORICA can be utilized to evaluate
(in)equality constrained hypotheses. The GORICA is applicable not only
to normal linear models, but also to generalized linear models (GLMs),
generalized linear mixed models (GLMMs), and structural equation models
(SEMs). In addition, the GORICA can be utilized in the context of
contingency tables for which (in)equality constrained hypotheses do not
necessarily contain linear restrictions on cell probabilities, but
instead often contain non-linear restrictions on cell probabilities.

## Installation

You can install gorica from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("cjvanlissa/gorica")
```

## Workflow

Add gorica to your existing R workflow, and evaluate informative
hypotheses for your familiar R analyses! Here is an example for testing
an informative hypothesis about mean differences in an ANOVA:

``` r
res <- lm(Sepal.Length ~ -1 + Species, data = iris)
gorica(res, "Speciessetosa < Speciesversicolor = Speciesvirginica; Speciessetosa < Speciesversicolor < Speciesvirginica")
#> Informative hypothesis test for an object of class lm:
#> 
#>    loglik  penalty gorica gorica_weights
#> H1 -14.948 1.500   32.897 0.000         
#> H2 5.103   1.834   -6.539 0.762         
#> Hu 5.103   3.000   -4.206 0.238         
#> 
#> Hypotheses:
#>   H1: Speciessetosa<Speciesversicolor=Speciesvirginica
#>   H2: Speciessetosa<Speciesversicolor<Speciesvirginica 
#>   Hu: Unconstrained hypothesis
#> 
```
