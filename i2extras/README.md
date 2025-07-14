
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/incidence2)](https://CRAN.R-project.org/package=i2extras)
[![R build
status](https://github.com/reconverse/i2extras/workflows/R-CMD-check/badge.svg)](https://github.com/reconverse/i2extras/actions)
[![Codecov test
coverage](https://codecov.io/gh/reconverse/i2extras/branch/master/graph/badge.svg)](https://app.codecov.io/gh/reconverse/i2extras?branch=master)
[![](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-maturing.svg)](https://www.reconverse.org/lifecycle.html#maturing)
<!-- badges: end -->

# Scope

*i2extras* adds additional functionality to the
[incidence2](https://github.com/reconverse/incidence2) package.

# What does it do?

The main features of the package include:

- `fit_curve()` and `growth_rate()`: fit a trend (poisson / negative
  binomial) to an `incidence2` object and calculate the associated
  growth rate.

- `add_rolling_average()`: add a rolling average to an `incidence2`
  object.

- `bootstrap()`: generates a bootstrapped `incidence2` object by
  re-sampling, with replacement, the original dates of events.

- `find_peak()`: locates the peak time of the epicurve.

- `estimate_peak()`: uses bootstrap to estimate the peak time (and
  related confidence interval) of a partially observed outbreak.

## Installing the package

You can install the released version of {i2extras} from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("i2extras")
```

<div class="pkgdown-devel">

The development version of {i2extras} can be installed from GitHub with:

``` r
remotes::install_github("reconverse/i2extras", build_vignettes = TRUE)
```

</div>

# Resources

## Vignettes

An overview of *i2extras* is provided in the included vignettes:

- `vignette("peak_estimation", package = "i2extras")`

- `vignette("fitting_epicurves", package = "i2extras")`

## Getting help online

Bug reports and feature requests should be posted on *github* using the
[*issue* system](https://github.com/reconverse/i2extras/issues). All
other questions should be posted on the **RECON** slack channel see
<https://www.repidemicsconsortium.org/forum/> for details on how to
join.
