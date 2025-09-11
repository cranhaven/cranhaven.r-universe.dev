
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NBDCtools

<!-- badges: start -->

<!-- badges: end -->

This R package provides functions for working with data released by the
[NIH Brain Development Cohorts (NBDC) Data
Hub](https://www.nbdc-datahub.org), which currently offers data for the
[ABCD](https://abcdstudy.org/) and [HBCD](https://hbcdstudy.org/)
studies.

The `NBDCtools` package is designed to help researchers and data
analysts create analysis-ready datasets from the tabulated data released
by the ABCD and HBCD studies. Its core functionality is to join selected
variables and/or entire tables from the tabulated data files into a
single data frame in memory. Additionally, the package offers several
functions to assist users in working with the data, including:

- Transformation functions to convert categorical columns to (ordered or
  unordered) factors based on the data dictionary and levels table, or
  to add variable and value labels to a dataset.
- Filter/subsetting functions to filter by a set of participant/events,
  filter ABCD events using shorthands, or exclude rows or columns with
  only missing data.
- Functions to retrieve and utilize metadata from the studies.
- A set of utility functions for various use cases.

## Installation

> **IMPORTANT:** Besides the `NBDCtools` main package, users also need
> to install the accompanying `NBDCtoolsData` package, which contains
> the large data dictionary and levels tables used by the `NBDCtools`
> package.

To install the *latest version* of the packages from
[GitHub](https://github.com/nbdc-datahub/NBDCtools), use the following
command:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
  
remotes::install_github("nbdc-datahub/NBDCtools")
remotes::install_github("nbdc-datahub/NBDCtoolsData")
```

There are some additional dependencies that are not installed by
default, as they are only needed for specific features, such as
processing shadow matrices. To install all the dependencies, use the
following command:

``` r
remotes::install_github("nbdc-datahub/NBDCtools", dependencies = TRUE)
```

To install a *specific version* of the package, specify the version
number in the `remotes::install_github()` call. For example, to install
version 1.0.0, use the following command:

``` r
remotes::install_github("nbdc-datahub/NBDCtools@v1.0.0")
remotes::install_github("nbdc-datahub/NBDCtoolsData@v1.0.0")
```

> **NOTE:** The `NBDCtoolsData` package version does not match the
> `NBDCtools` package version, i.e., the two package follow different
> release cycles. For example, the `NBDCtools` package may not be
> changed across ABCD/HBCD data releases, but the `NBDCtoolsData`
> package is updated with each data release. Therefore, it is
> recommended to always update the data package to the latest version.

Some package dependencies are not installed by default, as they are only
needed for specific features, such as processing shadow matrices. To
install these dependencies, use the following command:

``` r
# for shadow_bind_data function
install.packages("naniar")
```

## Usage

For a general overview of how to download data from the NBDC Data Hub
and use this R package to create custom datasets, see the [Get
Started](https://software.nbdc-datahub.org/NBDCtools/articles/NBDCtools.html)
page. To browse the documentation of all available functions, see the
[Reference](https://software.nbdc-datahub.org/NBDCtools/reference/index.html)
page. For more detailed information about different categories of
functions, refer to the
[vignettes](https://software.nbdc-datahub.org/NBDCtools/articles/).

## Issues

If you encounter issues while using this package, please report them by
submitting a [GitHub
issue](https://github.com/nbdc-datahub/NBDCtools/issues). Please do not
submit any issues related to the ABCD or HBCD data resources themselves
(see [here](https://nbdc.lassoinformatics.com/issue-tracker) for
information on how to report issues with the data resource).

## Citation

<!-- If you use this package for your research, please kindly cite the following paper: -->

*We are currently preparing a paper about `NBDCtools`. The reference
will be added here once the paper is published.*
