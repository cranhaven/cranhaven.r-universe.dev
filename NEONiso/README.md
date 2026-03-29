# NEONiso

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/188347333.svg)](https://zenodo.org/badge/latestdoi/188347333)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/NEONiso)](https://CRAN.R-project.org/package=NEONiso)
[![R-CMD-check](https://github.com/lanl/NEONiso/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lanl/NEONiso/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/lanl/NEONiso/branch/main/graph/badge.svg)](https://app.codecov.io/gh/lanl/NEONiso?branch=main)
<!-- badges: end -->

Author: Rich Fiorella
Latest release: February 25, 2025.

This repository contains an R package to calibrate NEON atmospheric isotope data. A stable version of the package can be installed from CRAN, and a development version of this package can be installed here using devtools (see below).

Please report any issues you have, bugs found, or enhancement suggestions as issues to this repository.

## Installing the development version:
1) NEONiso requires an HDF5 package. The recommended option is hdf5r from CRAN:
```R
install.packages("hdf5r")
```
Alternatively, rhdf5 from Bioconductor also works:
```R
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("rhdf5")
```
2) Install devtools, which is available on CRAN.
3) Install NEONiso from GitHub. Development version can be installed using:
```R
devtools::install_github("lanl/NEONiso")
```
Alternatively, you can install a specific version of the package (e.g., v0.1)
by specifying the version tag:
```R
devtools::install_github("lanl/NEONiso@v0.1")
```

## Citation information:
Package functionality to calibrate NEON carbon isotope data is described in a paper at JGR-Biogeosciences (doi: [10.1029/2020JG005862](https://doi.org/10.1029/2020JG005862)). Users of this package should also cite the Zenodo DOI above.

Please also check to ensure that you are compliant with NEON's data citation policy for any
products derived from this package: https://www.neonscience.org/data/about-data/data-policies

## Usage:

Two methods are available to calibrate NEON Carbon isotope data and they take slightly different approaches: a) the 'Bowling_2003' method calibrates 12CO2 and 13CO2 mole fractions independently, while b) the 'linreg' method calibrates d13C and CO2 directly without converting to isotopologue mole fractions. The method is specified as an argument to calibrate_carbon_bymonth(). Both methods yield very similar results, but the error and precision estimates are slightly better from the calibrate_carbon_Bowling2003() function (Fiorella et al., 2021; JGR-Biogeosciences)

This function is meant to be applied to a list or vector of uncalibrated data files, and produce output hdf5 files that have (currently) only the CO2 and d13C variables instead of the entire data bundle. Development was targeted and tested on monthly basic files, but the functions should also work on the extended data files.

neonUtilities:::stackEddy *should* work on these output files - please file an issue if it does not.

## DATA ALERT:

Several months of data on the NEON data portal have an issue where the Picarro time clock has diverged from the valve manifold time. A fix has been developed, but has not been propagated to the NEON data portal.


## Copyright notice:

© 2022. Triad National Security, LLC. All rights reserved.
This program was produced under U.S. Government contract 89233218CNA000001 for Los Alamos
National Laboratory (LANL), which is operated by Triad National Security, LLC for the U.S.
Department of Energy/National Nuclear Security Administration. All rights in the program are
reserved by Triad National Security, LLC, and the U.S. Department of Energy/National Nuclear
Security Administration. The Government is granted for itself and others acting on its behalf a
nonexclusive, paid-up, irrevocable worldwide license in this material to reproduce, prepare
derivative works, distribute copies to the public, perform publicly and display publicly, and to permit
others to do so.
