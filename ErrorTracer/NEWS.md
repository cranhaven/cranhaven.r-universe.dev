# ErrorTracer 1.1.0

## Breaking changes / deprecations

* `shelf_life()`: the `plausible_range` argument is deprecated. Use
  `response_scale` instead. `plausible_range` still works but emits a
  warning. It will be removed in a future release.

## New features

* `shelf_life()` now includes `se_t_star` in the projected-mode horizon
  attribute. This is the delta-method standard error of the projected
  crossing time `t* = (τ − a) / b`, propagating the linear-fit covariance.

* `decompose_uncertainty()` and the underlying `.decompose_from_arrays()`
  helper now support non-Gaussian families (Binomial, Poisson, Student-t,
  Negative-Binomial, Beta, Gamma) by computing all variance components on
  the response scale via the inverse link. For Gaussian identity the result
  is numerically unchanged.

* `et_predict()` gains an `n_env_draws` argument. Setting it > 1 averages
  multiple independent perturbations per posterior draw, reducing Monte
  Carlo noise in the environmental variance estimate. The decomposition
  data frame gains a `v_env_mcse` column reporting the chi-squared standard
  error of `env_var`.

* `decompose_uncertainty()` now reports a fourth `temporal_var` component
  when the model formula contains an autocorrelation term (`ar()`,
  `ma()`, `arma()`, `cosy()`, `unstr()`, `sar()`, or `car()`). The
  component is computed as `pmax(0, total_var - (param_var + env_var +
  residual_var))` and captures the autocorrelation-induced predictive
  variance beyond the iid sum, so the four components reconstruct
  `total_var` modulo Monte Carlo error. `residual_var` for autocor
  models is interpreted as the innovation variance (not the stationary
  marginal variance). `et_plot_decomposition()` adds a Temporal segment
  to the stacked bars automatically when the column is present, and
  `print.et_prediction()` includes the new row in its summary.

## Bug fixes

* `et_plot_calibration()` previously only recognised a column literally
  named `group` as the sub-group identifier. Calibration data frames built
  by binding per-group results with descriptive column names
  (e.g.\ `cluster_id`, `species`, `regime`) were silently collapsed into a
  single un-grouped series, producing plots with multiple overlapping
  points per nominal level and a zig-zagging connecting line. The function
  now auto-detects any single non-canonical column (anything other than
  `ci_level`, `nominal`, `observed_coverage`, `n_obs`, `calibration_error`,
  `sharpness`) with more than one unique value and uses it as the grouping
  variable. A new `group_col` argument allows the grouping column to be
  set explicitly, or `group_col = NA` to force a single un-grouped series.

# ErrorTracer 1.0.0

* Initial CRAN submission.

* Initial CRAN release.
* Added full Bayesian error propagation pipeline using `brms`.
* Added three-way variance decomposition and forecast shelf life metrics.
