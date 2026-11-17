
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mmequiv <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/mmequiv)](https://CRAN.R-project.org/package=mmequiv)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/mmequiv)](https://r-pkg.org/pkg/mmequiv)
[![R-CMD-check](https://github.com/KennethATaylor/mmequiv/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KennethATaylor/mmequiv/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/KennethATaylor/mmequiv/graph/badge.svg)](https://app.codecov.io/gh/KennethATaylor/mmequiv)

<!-- badges: end -->

The goal of `mmequiv` is to provide users the ability to calculate
standardized Morphine Milligram Equivalent (MME) doses for prescription
opioid medications for research purposes.

`mmequiv` uses information about prescription opioids used by study
participants to calculate MME and daily MME based on several
standardized definitions, including directly interfacing with the NIH
HEAL MME Online Calculator[^1] API from R. Because the NIH HEAL MME
Online Calculator API has a rate limit of 50 (patient-level) requests
per 15 minutes, `mmequiv` can also perform the same calculations locally
without needing to call the API when working with larger data or when
internet access is unavailable.

`mmequiv` is intended for the same purposes as the NIH HEAL MME Online
Calculator - as a data resource for: research, analytical purposes using
claims or dispensing data, and surveillance of population-level
medication utilization.

**Important**: Users should be aware of the caveats and limitations of
the NIH HEAL MME Online Calculator - these also apply to `mmequiv`:

> The data collection tool is ***NOT*** intended for any clinical
> decision-making by clinicians while prescribing opioids. The MME
> conversion factors in this survey ***DO NOT*** constitute any clinical
> guidance for prescribing or recommendations for converting patients
> from one form of opioid analgesic to another.

Much of the information in the `mmequiv` documentation is pulled
directly from either the NIH HEAL MME Online Calculator website or the
paper from the calculator’s creators.[^2] I recommend reviewing these
resources for any details not covered by the package documentation.

## Installation

You can install `mmequiv` from CRAN using the following code:

``` r
install.packages("mmequiv")
```

You can install the development version of `mmequiv` like so:

``` r
# install.packages("remotes")
remotes::install_github('KennethATaylor/mmequiv')
```

## Citation

In addition to citing the original paper by Adams, *et al*.,[^3]
consider citing this package. You can obtain information needed to cite
the package by running:

``` r
citation("mmequiv")
```

[^1]: Find the online version of the calculator at [this
    link](https://research-mme.wakehealth.edu/); accessible as of May
    19, 2025.

[^2]: Adams MCB, Sward KA, Perkins ML, Hurley RW. Standardizing research
    methods for opioid dose comparison: the NIH HEAL morphine milligram
    equivalent calculator. *PAIN*. Published online February 3, 2025.
    doi: <a href="https://doi.org/10.1097/j.pain.0000000000003529"
    class="uri">10.1097/j.pain.0000000000003529</a>

[^3]: Adams MCB, Sward KA, Perkins ML, Hurley RW. Standardizing research
    methods for opioid dose comparison: the NIH HEAL morphine milligram
    equivalent calculator. *PAIN*. Published online February 3, 2025.
    doi: <a href="https://doi.org/10.1097/j.pain.0000000000003529"
    class="uri">10.1097/j.pain.0000000000003529</a>
