# mshap 0.1.0

## Bug Fix

- Fix bug that was causing the expected values to be off if the list-mshap was passed as the second argument to `mshap()`

## Enhancement

- Some changes to paper code that is more accurate

# mshap 0.0.0.9007

## Enhancement

- Limit the number of variables that are plotted in the plotting functions
- Add data (.buildignore'd) and code pertaining to mshap paper

# mshap 0.0.0.9006

## Feature

- Added a vignette called `mshap_plots` that documents different ways to customize and use the plots.

## Enhancement

- Added examples to all function documentation.
- Added capability of observation plot to have character values in the `variable_values` data frame.

## Clean Up

- Made the warning message in `observation_plot()` go away (it was occurring because there was only one tick mark on each axis, so I added an invisible one)

# mshap 0.0.0.9005

## Feature

- Added parameters `colorscale`, `legend.position`, `font_family`, and `title` to the `summary_plot()` function, for greater ease of customization.
- Added parameters `fill_colors`, `connect_colors`, `expected_color`, `predicted_color`, `title`, and `font_family` to the `observation_plot()` function for greater ease of customization.
- Added Travis CI pipeline functionality.

## Enhancement

- Changed `{ggplot2}`, `{ggbeeswarm}` and `{tidyr}` to imports, instead of suggests.
- Added tests for all functions (close to 100% codecov).

## Bug Fix

- Fixed bug in `mshap()` function where data frames with the same number of columns but different names were not getting flagged to add additional columns even when `shap_*_names` was specified.

# mshap 0.0.0.9004

## Clean Up

- Adding dependencies and other items to ensure that there are no notes/warnings in `R CMD CHECK`

# mshap 0.0.0.9003

## Enhancement

- Added documentation to all functions

# mshap 0.0.0.9002

## Feature

- Added a helper function to make the `mshap` function more robust
- Finished the vignette

## Bug Fix

- Fixed bug in the summary_plot function with where the names were assigned

# mshap 0.0.0.9001

* Added a `NEWS.md` file to track changes to the package.
