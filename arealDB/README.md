
<!-- README.md is generated from README.Rmd. Please edit that file -->

# arealDB

[![CRAN
status](https://www.r-pkg.org/badges/version/arealDB)](https://cran.r-project.org/package=arealDB)
[![R-CMD-check](https://github.com/EhrmannS/arealDB/workflows/R-CMD-check/badge.svg)](https://github.com/luckinet/arealDB/actions)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![](http://cranlogs.r-pkg.org/badges/grand-total/arealDB)](https://cran.r-project.org/package=arealDB)

## Overview

Areal data are a rather frequent type of data in many applications of
the environmental and socio-economic sciences, where various aspects are
summarized for particular areas such as administrative territories. Many
of those applications surpass the spatial, temporal or thematic scope of
any single data source, so that data must be harmonised and normalised
across many distinct standards. `arealDB` has been developed for the
purpose of building a standardised database encompassing all issues that
come with this. In the current, revised version, it makes use of the
`ontologics` R-package to harmonise the names of territories (from
geometries) and the target variables (from tables). Moreover, it uses
the `tabshiftr` R-package to reshape disorganised tabular data into a
common format.

<figure>
<img
src="https://github.com/luckinet/arealDB/blob/master/vignettes/schematic_overview.png"
alt="Schematic overview of the result" />
<figcaption aria-hidden="true">Schematic overview of the
result</figcaption>
</figure>

## Installation

1)  Install the official version from CRAN:

``` r
install.packages("arealDB")
```

or the latest development version from github:

``` r
devtools::install_github("luckinet/arealDB")
```

2)  Read the [paper](https://doi.org/10.1016/j.envsoft.2020.104799) for
    a more scientific background, or study the vignette on [the arealDB
    pipeline](https://luckinet.github.io/arealDB/articles/arealDB.html).

## Getting started

To study how `arealDB` works, one can make use of the function
`makeExampleDB()`, where the full process of building an areal database
can be “simulated” with dummy data. This can be used to train yourself
on a particular step based on a fully valid database up until a certain
stage of the process. For instance, to set up database that has merely
just been started, but doesn’t contain any thematic data or geometries
yet, one would use
`makeExampleDB(path = paste0(tempdir(), "/newDB"), until = "start_arealDB")`.

In principle, `arealDB` follows a simple process involving three stages:

1.  **Setup the database (*stage 1*):** To start a new areal database,
    one needs to specify a gazetteer that contains the valid names of
    territories and optionally an ontology containing harmonised labels
    for the concepts in the thematic data.

2.  **Register data series, geometries and tables (*stage 2*):** A data
    item that shall be inserted into a database is registered by calling
    a register function, which records the configuration (to reorganise
    it internally into a common standard) of the file and meta-data.
    Just like the thematic data, which are typically in a table, the
    spatial data (geometries) and the data series are registered in that
    way.

3.  **Normalize geometries and tables (*stage 3*):** After registering
    all relevant data, they are reshaped into a standardized database
    format. In this process terms of territories and target variables
    are “translated” according to gazetteer and ontology, spatial data
    are standardized and validated, thematic data are standardized and
    matched to spatial data, and the spatial data are matched with the
    optionally already existing spatial database, for instance if that
    has been built off the GADM (recommended) or GAUL or other
    standardized datasets.

## Acknowledgement

This work was supported by funding to Carsten Meyer through the Flexpool
mechanism of the German Centre for Integrative Biodiversity Research
(iDiv) (FZT-118, DFG).
