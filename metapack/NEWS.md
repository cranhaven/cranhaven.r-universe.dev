# metapack 0.3.0
* Removed the C++11 requirement in response to an update in BH
* Updated the maintainer's e-mail address

# metapack 0.2.0
* Dots in function names are changed to underscores
* Changed the Wishart sampler from armadillo's to R's due to seeding and reproducibility issues

# metapack 0.1.5
* Fix bug in looping over the right dimension of Rho

# metapack 0.1.4
* Fix bug in `EquiCorrelation` model

# metapack 0.1.3
* Remove `random.cpp` and use `arma::wishrnd()`
* Add `bmeta_analyze()` with formula interface

# metapack 0.1.2
* Add DOI information for NMR-HTRe
* Increment `icount` for robust adaptive Metropolis algorithm for M4 (MVMR-ParObs)

# metapack 0.1.1
* Valgrind error fixed for Solaris
* Matrix initialized with memory allocation in `rwish` function
* Add Qunused-arguments to Makevars.win to suppress warnings for unused driver arguments

# metapack 0.1.0

## New features
+ The first version of **metapack** providing functions performing Bayesian inference for multivariate meta-regression and univariate network meta-regression models.