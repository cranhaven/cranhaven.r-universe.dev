## psborrow 0.2.4

* Change deprecated arguments in ggplot2 calls.

## psborrow 0.2.3

* Use `ggplot2::get_labs()` in tests due to change in functionality in that package.

## psborrow 0.2.2

* Fixed bug in apply_mcmc in subsetting a data frame when only one covariate is included

## psborrow 0.2.1

* psborrow package is now deprecated. All future development will occur in 
[psborrow2](https://github.com/Genentech/psborrow2).
* Fixed calculation of drift HR in analysis to match simulation

## psborrow 0.2.0

* Added `apply_mcmc()` function to make conducting analyses on non-simulated datasets easier
* Added "analysis" vignette detailing the analysis model being implemented
* Added `psborrow.quiet` option to suppress package messages
* Added `n.cores` argument to `run_mcmc_p()` to allow the user to choose how many cores are  used 
when running parallel processes
* Various documentation tweaks/clarifications
* Changed License to Apache 2.0

## psborrow 0.1.0

* Initial CRAN release

