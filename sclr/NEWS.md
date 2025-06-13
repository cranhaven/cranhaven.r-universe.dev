# sclr 0.3.1

- Updated `predict.sclr` to not have named columns with the [new version](https://github.com/tidyverse/tibble/blob/master/NEWS.md#tibble-299999014) of tibble. [fa696d9](https://github.com/khvorov45/sclr/commit/fa696d94a42add43287de2198e1873542dffa28d)

- Made linear predictor variance calculation faster in `predict.sclr`. [fa696d9](https://github.com/khvorov45/sclr/commit/fa696d94a42add43287de2198e1873542dffa28d)

# sclr 0.3.0

- Reparameterised the model so that all of the parameters are unconstrained.
New baseline is the logit transformation of the old baseline.

- Added the gradient ascent algorithm to handle cases with high baseline.

- Added a warning for a possible baseline of 1.

- Added the ability to check for a possible baseline of 1 with `check_baseline`.

- Added `logLik` method to access likelihood from the fit object.

- Added a warning message when the model is fit with no covariates.

# sclr 0.2.0

- Added `sclr_ideal_data` function to simulate ideal data for the model.

- Made simulations in data-raw self-contained.

- Added the ability to return parameter names that are more conventional (e.g.
"(Intercept)" instead of "beta_0"). See `conventional_names` argument in
`?sclr`.

- Made convergence stricter to avoid local maxima. Argument `n_conv` to `sclr`
and `sclr_fit` sets the number of times the algorithm has to converge. Best set
(the one with maximum likelihood) is chosen out of `n_conv` sets. Previously,
the algorithm only converged once.

- `sclr_log_likelihood` can now be called with a model matrix and a model response.

- Minor performance optimisations.

# sclr 0.1.0

First release.

## Main features

- Fits the scaled logit model using the Newton-Raphson method.

- Supports the predict method for the expected value of the linear beta X part
of the model.

- Can look for covariate values corresponding to a particular protection level.