
<!-- README.md is generated from README.Rmd. Please edit that file -->

# misha

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/misha)](https://CRAN.R-project.org/package=misha)
[![R-CMD-check](https://github.com/tanaylab/misha/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tanaylab/misha/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `misha` package is a toolkit for analysis of genomic data. it
implements an efficient data structure for storing genomic data, and
provides a set of functions for data extraction, manipulation and
analysis.

## Installation

You can install the released version of misha from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("misha")
```

And the development version from GitHub with:

``` r
remotes::install_github("tanaylab/misha")
```

## Usage

See the
[Genomes](https://tanaylab.github.io/misha/articles/Genomes.html)
vignette for instructions on how to create a misha database for common
genomes.

See the [user
manual](https://tanaylab.github.io/misha/articles/Manual.html) for more
usage details.

#### Running scripts from old versions of misha (\< 4.2.0)

Starting in `misha` 4.2.0, the package no longer stores global variables
such as `ALLGENOME` or `GROOT`. Instead, these variables are stored in a
special environment called `.misha`. This means that scripts written for
older versions of `misha` will no longer work. To run such scripts,
either add a prefix of `.misha$` to all those variables
(`.misha$ALLGENOME` instead of `ALLGENOME`), or run the following
command before running the script:

``` r
ALLGENOME <<- .misha$ALLGENOME
GROOT <<- .misha$GROOT
ALLGENOME <<- .misha$ALLGENOME
GINTERVID <<- .misha$GINTERVID
GITERATOR.INTERVALS <<- .misha$GITERATOR.INTERVALS
GROOT <<- .misha$GROOT
GWD <<- .misha$GWD
GTRACKS <<- .misha$GTRACKS
```
