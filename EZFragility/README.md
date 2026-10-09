
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EZFragility: Epileptogenic Zone Localization Based on neural Fragility EEG marker

[![](https://www.r-pkg.org/badges/version/EZFragility?color=orange)](https://cran.r-project.org/package=EZFragility)
[![](http://cranlogs.r-pkg.org/badges/grand-total/EZFragility?color=blue)](https://cran.r-project.org/package=EZFragility)
[![](https://img.shields.io/github/languages/code-size/Jiefei-Wang/EZFragility.svg)](https://github.com/Jiefei-Wang/EZFragility)
[![](https://img.shields.io/github/last-commit/Jiefei-Wang/EZFragility.svg)](https://github.com/Jiefei-Wang/EZFragility/commits/main)
[![R-CMD-check](https://github.com/Jiefei-Wang/EZFragility/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Jiefei-Wang/EZFragility/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Jiefei-Wang/EZFragility/graph/badge.svg)](https://app.codecov.io/gh/Jiefei-Wang/EZFragility)

## Introduction

The goal of this Rpackage is to allow neuroscientists to reproduce and
test the neural fragility method described in (Li et al. 2017, 2021).
This method implements an intracranial EEG (iEEG) marker of the
epileptogenic zone localization. In this method, seizures are
conceptualized as transitions from a stable networked system to an
unstable one. To quantify this, node fragility is computed from linear
network models, measuring each node’s susceptibility to destabilization.
There are significant details missing in (Li et al. 2017, 2021) to
reproduce the neural fragility method and adjust the parameters. This
Rpackage aims to identify and fill up the implementation details. It
will also allow users to test the method parameters on their data.

## Installation

To install the package from GitHub

``` r
devtools::install_github("Jiefei-Wang/EZFragility")
```

## EZFragility package tutorial

To load the package

``` r
library(EZFragility)
```

If you are working with the source code, you can load the package with

``` r
devtools::load_all()
```

The package contains an example data. To see the first 5 rows and
columns of the data, type

``` r
pt01EcoG[1:5, 1:5]
```

The package contains an example results. To see it, type

``` r
pt01Frag
```

For explanations on how to use the package please refer to the vignette.

``` r
vignette("Intro_to_EZFragility", package = "EZFragility")
```

## Implementation details

The method is based on building a discrete time linear system computing
a stable adjacency matrix A for the evolution of x(t).  
$x(t+1)=A x(t)$ with $x_i(t)$ the iEEG signal at time $t$ for electrode
$i$. A is computed for a series of time windows to derive the fragility
row.  
In this package, we are applying a ridge regression to solve the matrix
A. In (Li et al. 2017, 2021), a regularization parameter value of 1e-4
is recommended, however testing on the data from patient pt01 from the
Fragility data set (data subset available in this package) this value
does not ensure that A is always stable. To tackle this issue, we have
implemented a dichotomy to search for the lowest stable lambda value
rendering the matrix A stable (see R function ridgeSearch in file
ridge.r).

The method to compute the row perturbation is also not clear. To compute
the fragility row, a minimum 2-induced norm additive row perturbation
$\Delta$ is computed to destabilize the linear network placing an
eigenvalue of $A+\Delta$ at $\lambda=\sigma+j\omega$. The minimum norm
is a function of $\lambda$ given in (Li et al. 2017) (see function
fragilityRow in the scrip fragility.r), however the paper does not
describe how to choose $\lambda$ with $|\lambda|=1$. To tackle this
issue, we search for the value that minimize the norm of $\Delta$.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-LiFragility2021" class="csl-entry">

Li, Adam, Chester Huynh, Zhary Fitzgerald, Iahn Cajigas, and Damina
Brusko. 2021. “Neural Fragility as an EEG Marker of the Seizure Onset
Zone.” *Nature Neuroscience* 24 (10): 1465–74.
<https://doi.org/10.1038/s41593-021-00901-w>.

</div>

<div id="ref-LiFragility2017" class="csl-entry">

Li, Adam, Sara Inati, Kareem Zaghloul, and Srivedi Sarma. 2017.
*Fragility in Epileptic Networks: The Epileptogenic Zone*. Lecture Notes
in Computer Science. IEEE. <https://doi.org/10.23919/ACC.2017.7963378>.

</div>

</div>
