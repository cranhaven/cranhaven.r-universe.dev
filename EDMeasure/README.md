# EDMeasure

[![Travis-CI Build Status](https://travis-ci.org/zejin/EDMeasure.svg?branch=master)](https://travis-ci.org/zejin/EDMeasure.svg?branch=master)
[![](https://cranlogs.r-pkg.org/badges/grand-total/EDMeasure)](https://CRAN.R-project.org/package=EDMeasure)
[![](https://cranlogs.r-pkg.org/badges/EDMeasure)](https://CRAN.R-project.org/package=EDMeasure)

## Overview

**EDMeasure** provides measures of mutual dependence and tests of mutual independence,
independent component analysis methods based on mutual dependence measures,
and measures of conditional mean dependence and tests of conditional mean independence. 

The three main parts are:
- mutual dependence measures via energy statistics
  - measuring mutual dependence
  - testing mutual independence
- independent component analysis via mutual dependence measures
  - applying mutual dependence measures
  - initializing local optimization methods
- conditional mean dependence measures via energy statistics
  - measuring conditional mean dependence
  - testing conditional mean independence

## Mutual Dependence Measures via Energy Statistics

### Measuring mutual dependence

The mutual dependence measures include:
- asymmetric measure based on distance covariance
- symmetric measure based on distance covariance
- complete measure based on complete V-statistics
- simplified complete measure based on incomplete V-statistics
- asymmetric measure based on complete measure
- simplified asymmetric measure based on simplified complete measure
- symmetric measure based on complete measure
- simplified symmetric measure based on simplified complete measure

### Testing mutual independence

The mutual independence tests based on the mutual dependence measures are implemented as permutation tests.

## Independent Component Analysis via Mutual Dependence Measures

### Applying mutual dependence measures

The mutual dependence measures include:
- distance-based energy statistics 
  - asymmetric measure based on distance covariance
  - symmetric measure based on distance covariance
  - simplified complete measure based on incomplete V-statistics
- kernel-based maximum mean discrepancies
  - d-variable Hilbert−Schmidt independence criterion based on 
    Hilbert−Schmidt independence criterion

### Initializing local optimization methods

The initialization methods include:
- Latin hypercube sampling
- Bayesian optimization

## Conditional Mean Dependence Measures via Energy Statistics

### Measuring conditional mean dependence

The conditional mean dependence measures include:
- conditional mean dependence of Y given X
  - martingale difference divergence
  - martingale difference correlation
  - martingale difference divergence matrix
- conditional mean dependence of Y given X adjusting for the dependence on Z
  - partial martingale difference divergence
  - partial martingale difference correlation

### Testing conditional mean independence

The conditional mean independence tests include:
- conditional mean independence of Y given X conditioning on Z
  - martingale difference divergence under a linear assumption
  - partial martingale difference divergence

The conditional mean independence tests based on the conditional mean dependence measures are implemented as permutation tests.

## Installation

``` r
# Install the released version from CRAN
install.packages("EDMeasure")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("zejin/EDMeasure")
```




