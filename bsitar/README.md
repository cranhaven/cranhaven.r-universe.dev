## An overview

The **bsitar** package provides an interface for Bayesian implementation
of Super Imposition by Translation and Rotation (SITAR) growth model.
The SITAR is a shape-invariant nonlinear mixed effect model that fits a
natural cubic spline mean curve and aligns individual-specific growth
curves to the underlying mean curve via a set of random effects: the
size, timing and intensity. The **bsitar** package package is a
front-end to the R package **brms** which uses the **Stan** program to
performing full Bayesian inference for a range of regression
specifications including hierarchical multivariate modeling.

## Installation

To install the latest release version from CRAN use

``` r
install.packages("bsitar")
```

The current developmental version can be downloaded from GitHub via

``` r
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("Sandhu-SS/bsitar")
```

The **bsitar** internally depends on the **brms** package which can be
installed from the CRAN as

``` r
install.packages("brms")
```

The latest developmental version of **brms** can be downloaded from
GitHub as

``` r
remotes::install_github("paul-buerkner/brms")
```

Note that the brms, and hence the bsitar too, are based on Stan,
therefore  
a C++ compiler is required. The program Rtools (available on
<https://cran.r-project.org/bin/windows/Rtools/>) comes with a C++
compiler for Windows. On Mac, you should install Xcode. For further
instructions on how to get the compilers running, see the prerequisites
section on
<https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started>.
