
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GLMMcosinor <a href='https://docs.ropensci.org/GLMMcosinor/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![cranlogs](https://www.r-pkg.org/badges/version/GLMMcosinor)](https://CRAN.R-project.org/package=GLMMcosinor)
[![R-CMD-check](https://github.com/ropensci/GLMMcosinor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/GLMMcosinor/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/GLMMcosinor/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci/GLMMcosinor?branch=main)
[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Status at rOpenSci Software Peer
Review](https://badges.ropensci.org/603_status.svg)](https://github.com/ropensci/software-review/issues/603)
<!-- badges: end -->

The goal of `{GLMMcosinor}` is to fit a cosinor model to rhythmic data
with all the flexibility and functionality of a generalized linear
(mixed-) model (GLM) framework using `{glmmTMB}`.

The package is also accessible via a [web
app](https://aushsi.shinyapps.io/GLMMcosinor-shinyapp) developed using
shiny.

For an introduction to the cosinor model, see the [getting started
vignette](https://docs.ropensci.org/GLMMcosinor/articles/GLMMcosinor.html).

Existing statistical software for circadian data analyses (including
`cosinor` (Sachs 2023) or `circacompare` (Parsons et al. 2020)) allows
users to fit regression models to rhythmic data, but many are limited
due to their inability to specify a link function, multiple components,
or a hierarchical structure. `GLMMcosinor` aims to be comprehensive and
flexible and is an improvement on other implementations of cosinor model
fitting in R or Python. See table below for features available within
currently available methods.

`GLMMcosinor` makes use of the `glmmTMB` package framework for
estimation of the cosinor model. If the model has no random effects,
`glmmTMB` uses maximum likelihood estimation to estimate the linear
coefficients of the model. For models with random effects, a Laplace
approximation is used to integrate over the random effects. This
approximation is handled by the
[`TMB`](https://cran.r-project.org/package=TMB) package which uses
automatic differentiation of the joint likelihood function to
efficiently compute parameter estimates. A detailed explanation of this
process is described [here](https://doi.org/10.18637/jss.v070.i05)
(Kristensen et al. 2016).

<img src="man/figures/methods-table.png" width="=900px" alt="flextable formats" align="center" />

## Installation

You can install the development version of GLMMcosinor from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ropensci/GLMMcosinor")

# or, equivalently
install.packages("GLMMcosinor", repos = "https://ropensci.r-universe.dev")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(GLMMcosinor)
```

``` r
model <- cglmm(
  vit_d ~ X + amp_acro(time, group = "X", period = 12),
  data = vitamind
)
summary(model)
#> 
#>  Conditional Model 
#> Raw model coefficients:
#>                estimate standard.error   lower.CI upper.CI    p.value    
#> (Intercept)  29.6897986      0.4583696 28.7914106 30.58819 < 2.22e-16 ***
#> X1            1.9018605      0.7919688  0.3496302  3.45409   0.016331 *  
#> X0:main_rrr1  0.9307837      0.6260656 -0.2962822  2.15785   0.137089    
#> X1:main_rrr1  6.5102912      0.9303406  4.6868572  8.33373 2.6010e-12 ***
#> X0:main_sss1  6.2009927      0.6701952  4.8874342  7.51455 < 2.22e-16 ***
#> X1:main_sss1  4.8184563      0.8963299  3.0616821  6.57523 7.6259e-08 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Transformed coefficients:
#>                estimate standard.error    lower.CI upper.CI    p.value    
#> (Intercept) 29.68979858     0.45836964 28.79141059 30.58819 < 2.22e-16 ***
#> [X=1]        1.90186054     0.79196879  0.34963023  3.45409   0.016331 *  
#> [X=0]:amp1   6.27046001     0.66965643  4.95795753  7.58296 < 2.22e-16 ***
#> [X=1]:amp1   8.09946996     0.89570579  6.34391887  9.85502 < 2.22e-16 ***
#> [X=0]:acr1   1.42180626     0.09993555  1.22593618  1.61768 < 2.22e-16 ***
#> [X=1]:acr1   0.63715378     0.11493856  0.41187833  0.86243  2.966e-08 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
autoplot(model, superimpose.data = TRUE)
polar_plot(model)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

## Citation

``` r
citation("GLMMcosinor")
#> To cite package 'GLMMcosinor' in publications use:
#> 
#>   Parsons R, Jayasinghe O, White N, Rawashdeh O (2024). _GLMMcosinor:
#>   Fit a Cosinor Model Using a Generalized Mixed Modeling Framework_. R
#>   package version 0.2.1, https://docs.ropensci.org/GLMMcosinor/,
#>   <https://github.com/ropensci/GLMMcosinor>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {GLMMcosinor: Fit a Cosinor Model Using a Generalized Mixed Modeling Framework},
#>     author = {Rex Parsons and Oliver Jayasinghe and Nicole White and Oliver Rawashdeh},
#>     year = {2024},
#>     note = {R package version 0.2.1, 
#> https://docs.ropensci.org/GLMMcosinor/},
#>     url = {https://github.com/ropensci/GLMMcosinor},
#>   }
```

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-Kristensen2016TMB" class="csl-entry">

Kristensen, Kasper, Anders Nielsen, Casper W. Berg, Hans Skaug, and
Bradley M. Bell. 2016. “TMB: Automatic Differentiation and Laplace
Approximation.” *Journal of Statistical Software* 70 (5): 1–21.
<https://doi.org/10.18637/jss.v070.i05>.

</div>

<div id="ref-parsons2020circacompare" class="csl-entry">

Parsons, Rex, Richard Parsons, Nicholas Garner, Henrik Oster, and Oliver
Rawashdeh. 2020. “CircaCompare: A Method to Estimate and Statistically
Support Differences in Mesor, Amplitude and Phase, Between Circadian
Rhythms.” *Bioinformatics* 36 (4): 1208–12.

</div>

<div id="ref-sachs2023cosinor" class="csl-entry">

Sachs, Michael. 2023. *Cosinor: Tools for Estimating and Predicting the
Cosinor Model*. <https://CRAN.R-project.org/package=cosinor>.

</div>

</div>
