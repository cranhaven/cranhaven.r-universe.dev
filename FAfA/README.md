
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{FAfA}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

Install `{FAfA}` together with its dependencies from CRAN:

``` r
install.packages("FAfA")
```

The development version can be installed in the same way with `remotes`:

``` r
remotes::install_github("AFarukKILIC/FAfA", dependencies = TRUE)
```

## Run

You can launch the application by running:

``` r
FAfA::run_app()
```

## License

FAfA is distributed under the GNU Affero General Public License, version 3.
The Dynamic Fit Index integration acknowledges Melissa G. Wolf and Daniel
McNeish and their AGPL-3 `dynamic` R package, version 1.1.0. The integration
was rewritten for FAfA rather than copied verbatim. See `inst/COPYRIGHTS` for
the complete third-party notice. The complete FAfA source code is available
from <https://github.com/AFarukKILIC/FAfA>.

DFI method reference: McNeish, D., & Wolf, M. G. (2023). Dynamic fit index
cutoffs for confirmatory factor analysis models. *Psychological Methods,
28*(1), 61-88. <https://doi.org/10.1037/met0000425>

## About

You are reading the doc about version : 0.5

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-12-11 22:13:40 +03"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading FAfA
#> ── R CMD check results ─────────────────────────────────────────── FAfA 0.5 ────
#> Duration: 7.4s
#> 
#> ❯ checking package dependencies ... ERROR
#>   Namespace dependencies missing from DESCRIPTION Imports/Depends entries:
#>     'bsicons', 'bslib', 'ggplot2'
#>   
#>   Imports includes 27 non-default packages.
#>   Importing from so many packages makes the package vulnerable to any of
#>   them becoming unavailable.  Move as many as possible to Suggests and
#>   use conditionally.
#>   
#>   See section 'The DESCRIPTION file' in the 'Writing R Extensions'
#>   manual.
#> 
#> 1 error ✖ | 0 warnings ✔ | 0 notes ✔
#> Error: R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> Error in loadNamespace(x): there is no package called 'covr'
```
