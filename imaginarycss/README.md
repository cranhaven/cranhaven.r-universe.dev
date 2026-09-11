

<!-- README.md is generated from README.Rmd. Please edit that file -->

# imaginarycss: Tools for Studying Imaginary Cognitive Social Structure

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/imaginarycss)](https://CRAN.R-project.org/package=imaginarycss)
[![R-CMD-check](https://github.com/gvegayon/imaginarycss/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/gvegayon/imaginarycss/actions/workflows/r-cmd-check.yml)
[![CRANlogs downloads](https://cranlogs.r-pkg.org/badges/grand-total/imaginarycss)](https://cran.r-project.org/package=imaginarycss)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://github.com/gvegayon/imaginarycss/blob/master/LICENSE.md)
[![codecov](https://codecov.io/gh/gvegayon/imaginarycss/graph/badge.svg?token=ZB8FVLI7GN)](https://app.codecov.io/gh/gvegayon/imaginarycss)
[![status](https://tinyverse.netlify.app/badge//imaginarycss)](https://CRAN.R-project.org/package=imaginarycss)

<!-- badges: end -->

The `imaginarycss` package provides functions to measure and test
imaginary cognitive social structure (CSS) motifs, which are patterns of
perceived relationships among individuals in a social network. The
package includes tools for calculating motif frequencies, comparing
observed motifs to expected distributions, and visualizing motif
structures. It implements the methods described in Tanaka and Vega Yon
(2023) <DOI:10.1016/j.socnet.2023.11.005>.

When using this package, please cite the above article:

``` r
citation(package = "imaginarycss")
To cite the original research article where the methods were
introduced, use:

  Tanaka K, Vega Yon G (2024). "Imaginary Network Motifs: Structural
  Patterns of False Positives and Negatives in Social Networks."
  _Social Networks_, *78*, 65-80. doi:10.1016/j.socnet.2023.11.005
  <https://doi.org/10.1016/j.socnet.2023.11.005>,
  <https://www.sciencedirect.com/science/article/pii/S0378873323000813>.

And the actual R package:

  Najafzadehkhoei S, Vega Yon G, Tanaka K (2025). _imaginarycss: Tools
  for Studying Imaginary Cognitive Social Structure_. R package version
  0.1.0, <https://github.com/gvegayon/imaginarycss>.

To see these entries in BibTeX format, use 'print(<citation>,
bibtex=TRUE)', 'toBibtex(.)', or set
'options(citation.bibtex.max=999)'.
```

## Installation

## Installation

You can install the development version of `imaginarycss` from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("gvegayon/imaginarycss")
```

Or from <a href="https://gvegayon.r-universe.dev/"
target="_blank">R-universe</a> (recommended for the latest development
version):

``` r
install.packages(
  'imaginarycss',
  repos = c(
    'https://gvegayon.r-universe.dev',
    'https://cloud.r-project.org'
  )
)
```

Or from CRAN

``` r
install.packages("imaginarycss")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(imaginarycss)

source_ <- c(1, 2, 3, 1) 
target_ <- c(2, 1, 4, 4)

source_ <- c(source_, source_[-1] + 4)
target_ <- c(target_, target_[-1] + 4)

adjmat <- matrix(0L, nrow = 8, ncol = 8)
adjmat[cbind(source_, target_)] <- 1L

graph <- new_barry_graph(adjmat, n = 4)
graph
#> A barry_graph with 2 networks of size 4
#> .    .  1.00     .  1.00     .     .     .     . 
#>  1.00     .     .     .     .     .     .     . 
#>     .     .     .  1.00     .     .     .     . 
#>     .     .     .     .     .     .     .     . 
#>     .     .     .     .     .     .     .  1.00 
#>     .     .     .     .  1.00     .     .     . 
#>     .     .     .     .     .     .     .  1.00 
#>     .     .     .     .     .     .     .     .

# These are two attributes that are part of the barry_graph object
attr(graph, "endpoints")
#> [1] 8
attr(graph, "netsize")
#> [1] 4

count_recip_errors(graph)
#>    id                                    name value
#> 1   0    Partially false recip (omission) (0)     1
#> 2   0   Partially false recip (comission) (0)     0
#> 3   0   Completely false recip (omission) (0)     0
#> 4   0  Completely false recip (comission) (0)     0
#> 5   0            Mixed reciprocity errors (0)     0
#> 6   0                  (01) Accurate null (0)     3
#> 7   0  (02) Partial false positive (null) (0)     0
#> 8   0 (03) Complete false positive (null) (0)     0
#> 9   0 (04) Partial false negative (assym) (0)     0
#> 10  0                 (05) Accurate assym (0)     2
#> 11  0                    (06) Mixed assym (0)     0
#> 12  0 (07) Partial false positive (assym) (0)     0
#> 13  0 (08) Complete false negative (full) (0)     0
#> 14  0  (09) Partial false negative (full) (0)     1
#> 15  0                  (10) Accurate full (0)     0
```
## Code of Conduct

Please note that the imaginarycss project is released with a
[Contributor Code of Conduct](https://github.com/gvegayon/imaginarycss/blob/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.