
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CIMPLE

<!-- badges: start -->

<!-- badges: end -->

The goal of CIMPLE is to offer a collection of methods involved with
analyzing longitudinal EHR data with possibly informative observational
time. These methods are grouped into two classes depending on the
inferential task. One group focuses on estimating the effect of an
exposure on a longitudinal biomarker while the other group assesses the
impact of a longitudinal biomarker on time-to-diagnosis outcomes.

## Installation

You can install the development version of CIMPLE from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jiacongD/CIMPLE")
```

## Example

A very basic workflow:

``` r
library(CIMPLE)

time_var = "time"
id_var = "id"
outcome_var = "Y"
VPM_variables = c("Z", "X")
LM_fixedEffect_variables = c("Z", "X")
LM_randomEffect_variables = "Z"

# Run the standard LME model
fit_standardLME = long_est(
  long_data = train_data,
  method = "standard_LME",
  id_var = id_var,
  outcome_var = outcome_var,
  LM_fixedEffect_variables = LM_fixedEffect_variables,
  time = time_var,
  LM_randomEffect_variables = LM_randomEffect_variables,
  VPM_variables = VPM_variables
)
#> Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
#> unable to evaluate scaled gradient
#> Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
#> Model failed to converge: degenerate Hessian with 1 negative eigenvalues

# Return the coefficient estimates
fit_standardLME$beta_hat
#> (Intercept)           Z           X        time 
#> -2.18544419 -0.47282269  0.59100042  0.09982721
```
