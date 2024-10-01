
<!-- README.md is generated from README.Rmd. Make sure to edit the .Rmd file and not the .md -->
<!-- <img src='https://github.com/phylotastic/datelife/raw/master/man/figures/datelife-hexsticker-ai.png' align='right' style='width:150px' />

[![CRAN status](https://www.r-pkg.org/badges/version/datelife)](https://CRAN.R-project.org/package=datelife)
-->

![GitHub R package
version](https://img.shields.io/github/r-package/v/phylotastic/OpenTreeChronograms?color=green&label=GitHub)
[![R-CMD-check](https://github.com/phylotastic/OpenTreeChronograms/workflows/R-CMD-check/badge.svg)](https://github.com/phylotastic/OpenTreeChronograms/actions)
[![codecov](https://codecov.io/gh/phylotastic/datelife/branch/master/graph/badge.svg)](https://app.codecov.io/gh/phylotastic/OpenTreeChronograms)
[![Github Open
Issues](https://img.shields.io/github/issues-raw/phylotastic/OpenTreeChronograms.svg)](https://github.com/phylotastic/OpenTreeChronograms/issues)
[![Github Closed
Issues](https://img.shields.io/github/issues-closed-raw/phylotastic/OpenTreeChronograms.svg)](https://github.com/phylotastic/OpenTreeChronograms/issues?q=is%3Aissue+is%3Aclosed)
[![DOI](https://zenodo.org/badge/23036/phylotastic/OpenTreeChronograms.svg)](https://zenodo.org/badge/latestdoi/23036/phylotastic/OpenTreeChronograms)
[![NSF-1458603](https://img.shields.io/badge/NSF-1458603-white.svg)](https://nsf.gov/awardsearch/showAward?AWD_ID=1458603)
[![NSF-0905606](https://img.shields.io/badge/NSF-0905606-white.svg)](https://nsf.gov/awardsearch/showAward?AWD_ID=0905606)
[![NSF-1458572](https://img.shields.io/badge/NSF-1458572-white.svg)](https://nsf.gov/awardsearch/showAward?AWD_ID=1458572)

# OpenTreeChronograms: a database of phylogenetic trees with branch lengths proportional to time

Welcome to `OpenTreeChronograms` GitHub repository!

`OpenTreeChronograms` is an R package that hosts a chronogram database
assembled with data from the Open Tree of Life phylogenetic database,
the [Phylesystem](https://github.com/OpenTreeOfLife/phylesystem-1).

`OpenTreeChronograms` has been developed to work along with the
[`datelife` R package](http://phylotastic.org/datelife/index.html).

## README topics:

-   [Installation](#local)
-   [Loading](#loading)
-   [Citation](#citing)
-   [License](#license)
-   [Funding](#funding)

## Local installation of the `OpenTreeChronograms` R package

`OpenTreeChronograms`’s most recent stable version can be installed
with:

``` r
install.packages("OpenTreeChronograms")
```

`OpenTreeChronograms`’s previous stable versions are available for
installation from the CRAN repository. For example, to install
`version 2022.01.28`, you can run:

``` r
devtools::install_version("OpenTreeChronograms", version="2022.01.28")
```

You can install `OpenTreeChronograms`’s development version from its
GitHub repository with:

``` r
devtools::install_github("phylotastic/OpenTreeChronograms")
```

## Loading the database

``` r
data("opentree_chronograms", package = "OpenTreeChronograms")
```

``` r
opentree_chronograms$version 
```

\[1\] “2022.01.28”

``` r
opentree_chronograms$update
```

\[1\] “2022-01-28 15:24:53 PST”

## Citing `OpenTreeChronograms`

If you use `OpenTreeChronograms` for a publication, please cite the Open
Tree of Life database Phylesystem, the `OpenTreeChronograms` R package,
the accompanying paper, and all chronograms used.

## License

This package is free and open source software, licensed under GPL.

## Funding

`OpenTreeChronograms` was developed and is maintained as part of the
Datelife project, funded by the [phylotastic](http://phylotastic.org/)
and the [Open Tree of
Life](https://tree.opentreeoflife.org/about/open-tree-of-life) projects.
