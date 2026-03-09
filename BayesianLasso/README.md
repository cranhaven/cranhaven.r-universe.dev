
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BayesianLasso <img src="man/figures/logo.png" align="right" width="195"/>

<!-- badges: start -->

<!-- badges: end -->

BayesianLasso is an R package for efficient Bayesian inference in sparse
linear regression models using the Bayesian Lasso. It includes optimized
Gibbs sampling algorithms and utilities for working with the Lasso
distribution.

## Installation

You can install the development version of BayesianLasso from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("garthtarr/BayesianLasso")
```

## Features

- Efficient Gibbs samplers for Bayesian Lasso (e.g.,
  Modified_Hans_Gibbs, Modified_PC_Gibbs)

- Support for drawing from the Lasso distribution

- Utilities for computing moments and densities

## Example Usage

These are basic examples which show you how to solve a common problem:

``` r
library(BayesianLasso)
## basic example code

# Simulated data
set.seed(123)
X <- matrix(rnorm(100), 20, 5)
y <- rnorm(20)
beta_init <- rep(1, 5)

# Run modified Hans Gibbs sampler
result <- Modified_Hans_Gibbs(
  X = X,
  y = y,
  a1 = 0.01,
  b1 = 0.01,
  u1 = 0.01,
  v1 = 0.01,
  nsamples = 100,
  beta_init = beta_init,
  lambda_init = 0.1,
  sigma2_init = 1,
  verbose = 0
)

str(result)
#> List of 6
#>  $ mBeta   : num [1:100, 1:5] 0.2441 0.2277 0.2478 -0.1356 -0.0692 ...
#>  $ vsigma2 : num [1:100, 1] 0.913 0.767 0.704 0.747 0.623 ...
#>  $ vlambda2: num [1:100, 1] 34.96 87.38 9.41 53.49 68.44 ...
#>  $ mA      : num [1:100, 1:5] 18.4 20.1 24 26.1 24.6 ...
#>  $ mB      : num [1:100, 1:5] 5.67 3.15 3 2.11 2.49 ...
#>  $ mC      : num [1:100, 1:5] 0.1 6.19 10.68 3.66 8.46 ...
```

The `Modified_Hans_Gibbs()` function returns a list with the following
components:

- `mBeta`: MCMC samples of the regression coefficients
  $\boldsymbol{\beta}$, stored as a matrix with `nsamples` rows and `p`
  columns.
- `vsigma2`: MCMC samples of the error variance $\sigma^2$.
- `vlambda2`: MCMC samples of the global shrinkage parameter
  $\lambda^2$.
- `mA`, `mB`, `mC`: Matrices containing the MCMC samples of the Lasso
  distribution parameters $A_j$, $B_j$, and $C_j$ for each coefficient
  $\beta_j$, where each row corresponds to one MCMC iteration and each
  column to a regression coefficient.

## Lasso Distribution Functions

The package provides functions for working with the Lasso distribution:

- zlasso(): Normalizing constant

- dlasso(): Density function

- plasso(): CDF

- qlasso(): Quantile function

- rlasso(): Random generation

- elasso(): Expected value

- vlasso(): Variance

- mlasso(): Mode

- MillsRatio(): Mills ratio

## Citation

If you use this package in your work, please cite it appropriately.
Citation information can be found using:

``` r
citation("BayesianLasso")
#> To cite package 'BayesianLasso' in publications use:
#> 
#>   Ormerod J, Davoudabadi M, Tarr G, Mueller S, Tidswell J (2025). _Bayesian Lasso Regression and
#>   Tools for the Lasso Distribution_. R package version 0.3.0,
#>   <https://garthtarr.github.io/BayesianLasso/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {Bayesian Lasso Regression and Tools for the Lasso Distribution},
#>     author = {John Ormerod and Mohammad Javad Davoudabadi and Garth Tarr and Samuel Mueller and Jonathon Tidswell},
#>     year = {2025},
#>     note = {R package version 0.3.0},
#>     url = {https://garthtarr.github.io/BayesianLasso/},
#>   }
```
