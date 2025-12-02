
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The `rcdf` Package

<!-- badges: start -->

[![R-CMD-check](https://github.com/yng-me/rcdf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yng-me/rcdf/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/rcdf)](https://CRAN.R-project.org/package=rcdf)
<!-- badges: end -->

The `rcdf` package is a comprehensive toolkit for securely working with
RCDF (encrypted Parquet) files in R. RCDF is a custom data format
designed to provide strong encryption and metadata management for
sensitive datasets. With `rcdf`, users can easily handle encrypted data,
including reading, writing, and exporting data stored in this secure
format.

Key features include:

- **Secure data handling**: Functions for reading and writing encrypted
  Parquet files using AES and RSA encryption.
- **Metadata management**: Tools for handling and storing metadata in
  RCDF files, including automated key generation and decryption.
- **Parquet integration**: Full integration with the `arrow` package to
  read and write Parquet files seamlessly.
- **Cross-platform support**: Compatible with Linux, macOS, and Windows
  environments.

## Installation

Install from CRAN with:

``` r
install.packages("rcdf")
```

You can also install the development version of `rcdf` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yng-me/rcdf")
```

Once installed, you can load the package and start working with RCDF
files.

``` r
library(rcdf)
```
