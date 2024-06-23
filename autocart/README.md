# Autocart
[![DOI](https://zenodo.org/badge/270405538.svg)](https://zenodo.org/badge/latestdoi/270405538)

A modified regression tree R package that is intended for spatial datasets that feature coordinate information. Coordinate information is used to calculate measures of both spatial autocorrelation and spatial compactness during the splitting phase of the tree. This gives the tree more predictive power on these types of spatial datasets. The objective function for this regression tree is a linear combination of three objective functions. The hyperparameters "alpha" and "beta" (each given from 0.0 to 1.0 where alpha and beta do not sum to a number greater than 1.0) control the weight on the spatial autocorrelation and spatial compactness objective functions respectively.


## Installation from source
To install this package from the source code, make sure that you have the devtools package downloaded by using `install.packages("devtools")`.
### Windows
You must have Rtools downloaded so that the C++ source can be compiled. The most recent version of rtools can be found [here](https://cran.r-project.org/bin/windows/Rtools/)
### macOS
Install the Xcode command line tools with `xcode-select --install` in the shell. You may need to register as an Apple developer first.
### Linux
To compile the C++ code, you must also have the R development tools, which can be installed by installing the r-base-dev package.

### After downloading compiler
To install this package in an R environment, use `devtools::install_github("ethanancell/autocart")`

## Usage
To get started after installation, view the introductory autocart vignette by using `vignette("autocart-intro")`

## License
[MIT](LICENSE)
