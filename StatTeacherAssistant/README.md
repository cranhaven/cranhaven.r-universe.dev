
An application for creating, adjusting, and checking the suitability of
data sets for courses that incorporate introductory statistical methods

<!-- start badges -->
<!-- start badges -->

![](https://img.shields.io/badge/release-v0.0.1-blue?style=flat) [![CRAN
status](https://www.r-pkg.org/badges/version/StatTeacherAssistant)](https://CRAN.R-project.org/package=StatTeacherAssistant)
<!-- end badges -->

## Description

The `StatTeacherAssistant` `R` package includes an interactive Shiny
application, which is run locally on the user’s machine. It enables
users to randomly generate data, make new versions of existing data
through common adjustments (e.g., adding random normal noise and
performing transformations), and check the suitability of the resulting
data for statistical analyses. The app was designed to support educators
in wide-ranging disciplines, with a particular focus on those teaching
introductory statistical methods (descriptive and/or inferential) for
data analysis.

## Installation

The `StatTeacherAssistant` package can be installed from either
<a href="https://cran.r-project.org/" target="_blank">CRAN</a> or
<a href="https://github.com" target="_blank">GitHub</a>.

#### Installing from CRAN

To install from CRAN, run the following code in `R`:

``` r
install.packages("StatTeacherAssistant")
```

#### Installing from GitHub

To install the package from GitHub, run the following code in `R`:

``` r
install.packages("remotes")  # installs the remotes package for accessing the install_github() function
remotes::install_github("ccasement/StatTeacherAssistant")  # installs the StatTeacherAssistant package
```

## Usage

The `StatTeacherAssistant` application can be run using a single line of
code in `R`:

``` r
StatTeacherAssistant::runStatTeacherAssistantApp()
```

## Bug Reporting

If you happen to find any bugs, we kindly ask that you email us at
<casementc@gmail.com>.

## License

`StatTeacherAssistant` is distributed under the MIT license. For
details, see the LICENSE.md file.
