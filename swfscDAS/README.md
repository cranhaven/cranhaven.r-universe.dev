# swfscDAS

<!-- badges: start -->

[![CRAN version](http://www.r-pkg.org/badges/version/swfscDAS)](https://cran.r-project.org/package=swfscDAS)
[![R-CMD-check](https://github.com/swfsc/swfscDAS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/swfsc/swfscDAS/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package contains functions designed for processing and analyzing shipboard DAS data. This package was originally designed to handle [DAS data](https://swfsc-publications.fisheries.noaa.gov/publications/TM/SWFSC/NOAA-TM-NMFS-SWFSC-305.PDF) collected using WinCruz, the ship-based line-transect data collection software developed by the Southwest Fisheries Science Center. However, the `swfscDAS` package can be used by anyone who has collected DAS data that meets the format requirements described in the ['DAS data format'](#DAS-data-format) section below, for instance to process decades of available NOAA Fisheries DAS survey data.

More generally, the package is intended to standardize and streamline basic DAS data processing. Functionality currently includes reading DAS data into a data frame, processing this data (extracting state and condition information for each DAS event), and summarizing sighting, effort, and comment information. Learn more in `vignette("swfscDAS")`

## Installation

You can install the released version of swfscDAS from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("swfscDAS")
```

You can install the developmental version of swfscDAS from [GitHub](https://github.com) with:

``` r
# install.packages("remotes")
remotes::install_github("swfsc/swfscDAS")
```

## DAS data format

swfscDAS expects data to follow the conventions and format used by the WinCruz program. You can [download a PDF here](https://github.com/swfsc/swfscDAS/blob/main/inst/DAS_Format.pdf) describing the DAS data format requirements. See `das_format_pdf` for instructions on how to access the local copy of the format PDF that is included in the package.

## Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
