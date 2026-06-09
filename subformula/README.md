
<!-- README.md is generated from README.Rmd. Please edit that file -->

# subformula <img src="man/figures/logo.png" align="right" width="200" height="67" />

<!-- badges: start -->

[![Build
Status](https://travis-ci.com/JonasMoss/subformula.svg?branch=master)](https://travis-ci.com/JonasMoss/subformula)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/JonasMoss/subformula?branch=master&svg=true)](https://ci.appveyor.com/project/JonasMoss/subformula)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/subformula)](https://cran.r-project.org/package=subformula)
[![Coverage
Status](https://codecov.io/gh/JonasMoss/subformula/branch/master/graph/badge.svg)](https://codecov.io/gh/JonasMoss/subformula?branch=master)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- badges: end -->

`Subformula` constructs the subformulas of a `formula` object. Use this
to speed up tasks such as model selection and comparison of models.

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JonasMoss/subformula")
```

## Usage

There are two functions in the package, `subformula` and `fapply`.
`subformulas` creates subformulas while `fapply` is a member of the
`apply` family tailored for fitting a statistical model for each formula
in a list of formulas.

A formula `sub` is a subformula of `formula` if *(i)* all the terms on
the right hand side of `sub` are terms of `form` and *(ii)* their left
hand sides are identical. `subformula` finds every subformula of
`formula` that contains each term in `protected`.

``` r
library("subformula")
formula = mpg ~ wt + gear + cyl
subformula(formula, protected = ~ cyl)
#> [[1]]
#> mpg ~ cyl
#> 
#> [[2]]
#> mpg ~ wt + cyl
#> 
#> [[3]]
#> mpg ~ gear + cyl
#> 
#> [[4]]
#> mpg ~ wt + gear + cyl
```

Now apply the subformulas to a `model` function such as `lm` with
`fapply`.

``` r
models = fapply(subformula(formula, protected = ~ cyl), lm, data = mtcars)
lapply(models, AIC)
#> $`mpg ~ cyl`
#> [1] 169.3064
#> 
#> $`mpg ~ wt + cyl`
#> [1] 156.0101
#> 
#> $`mpg ~ gear + cyl`
#> [1] 170.7377
#> 
#> $`mpg ~ wt + gear + cyl`
#> [1] 157.4991
```

## How to Contribute or Get Help

If you encounter a bug, have a feature request or need some help, open a
[Github issue](https://github.com/JonasMoss/subformula/issues). This
project follows a [Contributor Code of
Conduct](https://www.contributor-covenant.org/version/1/4/code-of-conduct.html).
