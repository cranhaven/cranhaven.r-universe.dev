<!-- badges: start -->
[![R-CMD-check](https://github.com/cdueben/cppcontainers/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cdueben/cppcontainers/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# cppcontainers

The package makes C++ Standard Template Library containers interactively usable in R.

## Advantages

A brief summary of the discussion in the vignette:
- Speed up development workflows.
- Use C++ data structures without a compiler.
- Make C++ containers easily accessible to R users.
- Push R towards general purpose language.
- Bring performance of C++ containers to R.

## Installation

An installation from source requires compilation. On Windows, you must, therefore, have Rtools installed.

Install the package from GitHub:
```
devtools::install_github("cdueben/cppcontainers", build_vignettes = T)
```

On various operating systems, you can skip the compilation step by installing the binary package from 
[CRAN](https://CRAN.R-project.org/package=cppcontainers):
```
install.packages("cppcontainers")
```

## User Guide

New users may want to consult the vignette for an introduction to the package. It, e.g., briefly introduces the container types and lists which method 
applies to which class.

## Contribution

You are welcome to contribute by reporting issues on GitHub or by extending the package through pull requests. Guidelines on coding style and project 
organization are in inst/guidelines.md.
