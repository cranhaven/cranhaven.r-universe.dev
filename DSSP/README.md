
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DSSP

<!-- badges: start -->

[![R-CMD-check](https://github.com/gentrywhite/DSSP/workflows/R-CMD-check/badge.svg)](https://github.com/gentrywhite/DSSP/actions)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-month/DSSP)](https://www.r-pkg.org/pkg/DSSP)
[![arXiv](https://img.shields.io/badge/arXiv-1906.05575-00ff00.svg)](https://arxiv.org/abs/1906.05575)
<!-- badges: end -->

The goal of DSSP is to draw samples from the direct sampling spatial
prior model (DSSP) described in White et. al. 2019. The basic model
assumes a Gaussian likelihood and derives a spatial prior based on
thin-plate splines. Functions are included so that the model can be
extended to be used for generalised linear mixed models or Bayesian
Hierarchical Models.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gentrywhite/DSSP")
```

## Example

Short example using the Meuse dataset from the `{gstat}` package.

``` r
data("meuse.all", package = "gstat")
sp::coordinates(meuse.all) <- ~ x + y
```

This model does includes two covariates and their posterior densities
are summarised in the `summary()`

``` r
library(DSSP)
meuse.fit <- DSSP(
  formula = log(zinc) ~ log(lead) + lime, data = meuse.all, N = 10000,
  pars = c(0.001, 0.001), log_prior = function(x) -2 * log(1 + x)
)
summary(meuse.fit)
#> Formula: log(zinc) ~ log(lead) + lime
#> Number of observations: 164
#> Number of iterations: 10000
#> 
#> Summary of model:
#>       Estimate Est.Error l-95% CI u-95% CI      ESS
#> eta     334.14   1202.47     3.61  1897.49 10000.00
#> delta     0.11      0.01     0.09     0.14 10000.00
#> 
#> Summary of covariates:
#>             Estimate Est.Error  min q0.025 q0.25 q0.50 q0.75 q0.975  max
#> (Intercept)     1.25      0.20 0.48   0.89  1.12  1.25  1.38   1.67 2.30
#> log(lead)       0.96      0.04 0.84   0.89  0.94  0.96  0.99   1.03 1.10
#> lime            0.24      0.05 0.03   0.14  0.20  0.24  0.27   0.34 0.43
```

We can inspect several plots for the model using `plot()`.

``` r
plot(meuse.fit)
#> `geom_smooth()` using formula 'y ~ x'
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-4-2.png" width="100%" />

## Introduction

The Direct Sampling Spatial Prior (DSSP) is based on the thin-plate
splines solution to the smoothing problem of minimising the penalised
sum of squares

![
S\_{\\eta}(f) = \\frac{1}{n}\\sum^{n}\_{i}W_i(y_i - f(\\mathbf{x}\_i))^2
+\\eta J_m(f)
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0AS_%7B%5Ceta%7D%28f%29%20%3D%20%5Cfrac%7B1%7D%7Bn%7D%5Csum%5E%7Bn%7D_%7Bi%7DW_i%28y_i%20-%20f%28%5Cmathbf%7Bx%7D_i%29%29%5E2%0A%2B%5Ceta%20J_m%28f%29%0A "
S_{\eta}(f) = \frac{1}{n}\sum^{n}_{i}W_i(y_i - f(\mathbf{x}_i))^2
+\eta J_m(f)
")

which can be written as

![
\\min\_{\\mathbf{\\nu}}\\:(\\mathbf{y}-\\mathbf{\\nu})'\\mathbf{W}(\\mathbf{y}-\\mathbf{\\nu})+
\\eta\\mathbf{\\nu}'\\mathbf{M}\\mathbf{\\nu}.
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cmin_%7B%5Cmathbf%7B%5Cnu%7D%7D%5C%3A%28%5Cmathbf%7By%7D-%5Cmathbf%7B%5Cnu%7D%29%27%5Cmathbf%7BW%7D%28%5Cmathbf%7By%7D-%5Cmathbf%7B%5Cnu%7D%29%2B%0A%5Ceta%5Cmathbf%7B%5Cnu%7D%27%5Cmathbf%7BM%7D%5Cmathbf%7B%5Cnu%7D.%0A "
\min_{\mathbf{\nu}}\:(\mathbf{y}-\mathbf{\nu})'\mathbf{W}(\mathbf{y}-\mathbf{\nu})+
\eta\mathbf{\nu}'\mathbf{M}\mathbf{\nu}.
")

The solution for this problem is

![
\\hat{\\mathbf{\\nu}}=
(\\mathbf{W}+\\eta\\mathbf{M})^{-1}\\mathbf{y}.
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Chat%7B%5Cmathbf%7B%5Cnu%7D%7D%3D%0A%28%5Cmathbf%7BW%7D%2B%5Ceta%5Cmathbf%7BM%7D%29%5E%7B-1%7D%5Cmathbf%7By%7D.%0A "
\hat{\mathbf{\nu}}=
(\mathbf{W}+\eta\mathbf{M})^{-1}\mathbf{y}.
")

If we assume that the observed data are from a Gaussian distribution

![
\\mathbf{y}\\sim N(\\mathbf{\\nu},\\delta\\mathbf{W}^{-1})
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cmathbf%7By%7D%5Csim%20N%28%5Cmathbf%7B%5Cnu%7D%2C%5Cdelta%5Cmathbf%7BW%7D%5E%7B-1%7D%29%0A "
\mathbf{y}\sim N(\mathbf{\nu},\delta\mathbf{W}^{-1})
")

and if we specify the prior for
![\\mathbf{\\nu}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmathbf%7B%5Cnu%7D "\mathbf{\nu}")

![
\\left\[\\mathbf{\\nu}\\mid\\eta,\\delta\\right\]\\propto\\ \\frac{\\eta}{\\delta}^{-{r}/2}
\\exp\\left(-\\frac{\\eta}{2\\delta}\\mathbf{\\nu}'\\mathbf{M}\\mathbf{\\nu}\\right),
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cleft%5B%5Cmathbf%7B%5Cnu%7D%5Cmid%5Ceta%2C%5Cdelta%5Cright%5D%5Cpropto%5C%20%5Cfrac%7B%5Ceta%7D%7B%5Cdelta%7D%5E%7B-%7Br%7D%2F2%7D%0A%5Cexp%5Cleft%28-%5Cfrac%7B%5Ceta%7D%7B2%5Cdelta%7D%5Cmathbf%7B%5Cnu%7D%27%5Cmathbf%7BM%7D%5Cmathbf%7B%5Cnu%7D%5Cright%29%2C%0A "
\left[\mathbf{\nu}\mid\eta,\delta\right]\propto\ \frac{\eta}{\delta}^{-{r}/2}
\exp\left(-\frac{\eta}{2\delta}\mathbf{\nu}'\mathbf{M}\mathbf{\nu}\right),
")

the resulting posterior of
![\\nu](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cnu "\nu")
is proportional to

![
-\\frac{1}{2\\delta}\\left( (\\mathbf{y}-\\mathbf{\\nu})'\\mathbf{W}({\\mathbf{y}}-\\mathbf{\\nu})
-\\eta\\mathbf{\\nu}'\\mathbf{M}\\mathbf{\\nu}\\right)
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A-%5Cfrac%7B1%7D%7B2%5Cdelta%7D%5Cleft%28%20%28%5Cmathbf%7By%7D-%5Cmathbf%7B%5Cnu%7D%29%27%5Cmathbf%7BW%7D%28%7B%5Cmathbf%7By%7D%7D-%5Cmathbf%7B%5Cnu%7D%29%0A-%5Ceta%5Cmathbf%7B%5Cnu%7D%27%5Cmathbf%7BM%7D%5Cmathbf%7B%5Cnu%7D%5Cright%29%0A "
-\frac{1}{2\delta}\left( (\mathbf{y}-\mathbf{\nu})'\mathbf{W}({\mathbf{y}}-\mathbf{\nu})
-\eta\mathbf{\nu}'\mathbf{M}\mathbf{\nu}\right)
")

which yields the posterior mean with the same solution as the penalised
least squares.

The complete model is specified with a Gaussian likelihood, the improper
prior for
![\\nu](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cnu "\nu"),
an inverse gaussian prior for
![\\delta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cdelta "\delta"),
and a prior for
![\\eta](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Ceta "\eta").
With this specification the joint posterior is written

![
\\pi\\left(\\mathbf{\\nu},\\delta_0,\\eta\|\\mathbf{y}\\right)\\propto
f(\\mathbf{y}\|\\mathbf{\\nu},\\delta)\\pi\\left(\\mathbf{\\nu}\|\\eta,\\delta\\right)\\pi\\left(\\delta\\right)
\\pi\\left(\\eta\\right).
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cpi%5Cleft%28%5Cmathbf%7B%5Cnu%7D%2C%5Cdelta_0%2C%5Ceta%7C%5Cmathbf%7By%7D%5Cright%29%5Cpropto%0Af%28%5Cmathbf%7By%7D%7C%5Cmathbf%7B%5Cnu%7D%2C%5Cdelta%29%5Cpi%5Cleft%28%5Cmathbf%7B%5Cnu%7D%7C%5Ceta%2C%5Cdelta%5Cright%29%5Cpi%5Cleft%28%5Cdelta%5Cright%29%0A%5Cpi%5Cleft%28%5Ceta%5Cright%29.%0A "
\pi\left(\mathbf{\nu},\delta_0,\eta|\mathbf{y}\right)\propto
f(\mathbf{y}|\mathbf{\nu},\delta)\pi\left(\mathbf{\nu}|\eta,\delta\right)\pi\left(\delta\right)
\pi\left(\eta\right).
")

Given this it is possible to derive the set of posterior distributions

![\\pi\\left(\\mathbf{\\nu}\|\\delta,\\eta,\\mathbf{y}\\right)\\\\](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cpi%5Cleft%28%5Cmathbf%7B%5Cnu%7D%7C%5Cdelta%2C%5Ceta%2C%5Cmathbf%7By%7D%5Cright%29%5C%5C "\pi\left(\mathbf{\nu}|\delta,\eta,\mathbf{y}\right)\\")

![\\pi\\left(\\delta\|\\eta,\\mathbf{y}\\right)\\\\](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cpi%5Cleft%28%5Cdelta%7C%5Ceta%2C%5Cmathbf%7By%7D%5Cright%29%5C%5C "\pi\left(\delta|\eta,\mathbf{y}\right)\\")

![\\pi\\left(\\eta\|\\mathbf{y}\\right)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cpi%5Cleft%28%5Ceta%7C%5Cmathbf%7By%7D%5Cright%29 "\pi\left(\eta|\mathbf{y}\right)")

which can be sampled directly in sequence to create a draw from the
joint posterior

![
\\pi\\left(\\mathbf{\\nu},\\delta_0,\\eta\|\\mathbf{y}\\right).
](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%0A%5Cpi%5Cleft%28%5Cmathbf%7B%5Cnu%7D%2C%5Cdelta_0%2C%5Ceta%7C%5Cmathbf%7By%7D%5Cright%29.%0A "
\pi\left(\mathbf{\nu},\delta_0,\eta|\mathbf{y}\right).
")

This is the heart of what the function `DSSP()` does[^1].

[^1]: see G. White, D. Sun, P. Speckman (2019) \<arXiv:1906.05575\> for
    details
