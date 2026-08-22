# whatifbandit 1.0.2

* CRAN Submission

# whatifbandit 1.0.1

## Minor Fixes
* Fixing an error in contrast estimation for `"control"` when `control_augment` = 0 in `mab_from_rct()`
* Changed estimator "AIPW" to "AW-AIPW" to emphasize adaptive weighting.=

# whatifbandit 1.0.0

## Breaking Changes
* `multiple_mab_simulation()` and `single_mab_simulation()` are removed in favor of `mab_from_rct()`
  which performs the same functions. Multiple simulations specified via the `r` argument.
* `mab_from_rct()` now accepts bare column names and no longer accepts strings
* `multiple.mab` and `mab` classes have been removed in favor of `multi_rct_mab`, `multi_param_mab`,
  `single_rct_mab`, `single_param_mab`, and `.mab`.
* `plot()`, `summary()`, and `print()` methods have been removed pending new versions.

## New Features
* Joint hypothesis testing is now provided by `joint_test()` a function accepting `single_*_mab`
  classes. Methods are `"bootstrap"` and `"randomization"` inspired by Offer-Westort et. al (2021),
  see docs for more.
* `simulate_mab()` is a new function that simulates a Multi-Arm-Bandit trial from provided
  population parameters.
  Uses similar options to `mab_from_rct()`. See the `simulate_mab()` documentation for details.
*  Inverse Probability Weighted estimates (IPW) are now provided, and cluster-robust standard errors
   supported
* Linear contrast estimation supported. `simulate_mab()` and `mab_from_rct()` can now produce linear
  contrasts (e.g. treatment effects) in final output along with traditional mean
  estimates for all estimators. See the documentation for which options are available.
* New discount rate parameter: Information from previous periods is now weighted by a discount rate,
  which can be useful for non-stationary bandits. See `simulate_mab()` and `mab_from_rct()`
  documentation for more info.

## Other Changes
* `random_assign_prop` now adjusts the assignment probabilities instead of splitting the data; see
  docs for more details.
* Updating UCB1 formula and assignment mechanism to match the canonical paper Auer et al. (2002)
* Major internal optimizations for runtime. Runtime decreased for packaged `tanf` dataset with individual
  assignment from 20-30 seconds on v.0.3.0 to 9 seconds.
* Vignette has been removed pending new version.

## Bug Fixes
* Aligning output structure for `data.table` versus `tibble` data.frame classes. Ensuring results
  are data.frame library agnostic. Fixing miscellaneous bugs for `data.table` inputs.

# whatifbandit 0.3.0

## Breaking Change
* `plot.mab()` for `type = "assign"` now displays the proportion of total observations assigned to each treatment for each period, instead of the individual probability of assignment.

## New Features
* `multiple_mab_simulation()` calculates the number of observations assigned to each treatment for each trial, and provides support for plotting them has been added to `plot.multiple.mab()` via the `type = "hist"` and `quantity = "assignment"` arguments.
* `summary.mab()` now includes a new column with the number of observations assigned to each treatment.
* `summary.multiple.mab()` now includes two new columns with the mean and standard deviation for the number of observations assigned to each treatment across the simulations.
* Month-based assignment, `time_unit = "month"` can now be specified with and without an appropriate `month_col`, resulting in either time-based (no `month_col`) or calendar-based (provided `month_col`) assignments. See the `time_unit` documentation for
more details.
* `plot.multiple.mab()` now accepts arguments for `ggplot2::facet_grid()` for more precise customizations.

## Other
* First submission to CRAN

# whatifbandit 0.2.1

## Bug Fixes
* Fixed handling of numeric and factor types in `condition_col` of the `data_cols` argument.
* Weighting AIPW by group size along with adaptive weights.
* Fixed inconsistent results across with data.frames, tibbles, and data.tables. Running `single_mab_simulation()` or `multiple_mab_simulation()`, with
the same seeds on the same system, now results in the same outcome regardless of input data class.

# whatifbandit 0.2.0

## New Features
* `multiple_mab_simulation()` supports parallel processing via [future](https://future.futureverse.org/).
* `single_mab_simulation()` and `multiple_mab_simulation()` support
[data.table](https://cran.r-project.org/package=data.table) for larger data sets.
* `summary()`, `print()`, and `plot()` generics for `mab` and `multiple.mab` class objects.
* `single_mab_simulation()` and `multiple_mab_simulation()` throw informative
error messages, relating to argument specification, and data types passed.

# whatifbandit 0.1.1

## Bug Fixes
* Fixed AIPW calculations mistakes.
* Fixed improper random seeding in `multiple_mab_simulation()`.
* Improved numerical calculation errors in Thompson sampling with large datasets.
* Optimization reduced simulation runtime by up to 50%.


# whatifbandit 0.1.0
* `single_mab_simulation()` and `multiple_mab_simulation()` simulate successfully.
