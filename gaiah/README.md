
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gaiah

<!-- badges: start -->

[![R-CMD-check](https://github.com/eriqande/gaiah/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/eriqande/gaiah/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This is an R-package for performing **G**enetic **A**nd **I**sotopic
**A**ssignment using **H**abitat features.

Most of the package has been written by Eric C. Anderson. The
methodology for assigning individuals using stable isotope was first
written by Hannah Vander Zanden and colleagues. It was reimplemented
here and is now efficient enough to do leave one out cross validation,
etc. These parts of the code are in the file
`R/vander-zanden-appendix.R`.

## Installing

To install the development version from GitHub with the `devtools`
package, you can:

``` r
devtools::install_github("eriqande/gaiah")
```

## Vignettes?

In lieu of a vignette, we provide the full source code for our use of
the `gaiah` package for a recent paper. See below.

## Reproducing the results from Ruegg et al. (2017)

If you want to reproduce all the results from the paper “Genetic
assignment with isotopes and habitat suitability (GAIAH), a migratory
bird case study” in *Methods in Ecology and Evolution* you will have to
get the `gaiah` package and then also clone the analysis repository from
<https://github.com/eriqande/gaiah-wiwa> and then follow the
instructions in [that repository’s
README](https://github.com/eriqande/gaiah-wiwa/blob/master/README.md).
