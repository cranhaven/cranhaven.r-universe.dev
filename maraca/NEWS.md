# maraca 0.7

## New features

- The maraca package has been re-factored to allow flexibility in the type of
  outcomes that can be visualized. The user can now also include binary endpoints
  in their hierarchical endpoint. Details are given in the new vignette
  "Maraca Plots - Alternative Endpoints".
- Additionally to the `component_plot()`, there has been a new plot added called
  `cumulative_plot()`. As opposed to the previous plot showing the individual
  components of the win odds computation, this plot is displaying 
  the endpoints cumulated instead (adding one component of hierarchical endpoint
  at a time). Details can be found in the vignette "Maraca Plots - Plotting win odds".

## Parameter change
- As part of the re-factoring to allow for other endpoint types, the parameter
  `tte_outcomes` has been changed to `step_outcomes` and the parameter
  `continuous_outcome` to `last_outcome`.

## Dependency change

- The `ggplot2` is now automatically attached when loading `maraca`.
- `maraca` has a new dependency - the `patchwork` package.

## Bug fixes

- The `trans` parameter in the plotting functions was not working as
  intended. It now enables x-axis transformation for the continuous
  endpoint part of the plot.

# maraca 0.6

## New features

- The `theme` argument in the plotting functions allows users to easily change the
  styling of the plot. Details are given in the new vignette
  "Maraca Plots - Themes and Styling".
- A new plot to visualize the win odds components was added. The `component_plot()`
  function works for both `maraca` and `hce`. Details can be found in the new
  vignette "Maraca Plots - Plotting win odds".
- A new vignette called "Maraca Plots - Validation" to highlight the function
  `validate_maraca()` that was added in version 0.5.

## Dependency change

- `maraca` now has increased the version dependency for the package `hce`
  to >= 0.5.
- The `hce` package is now automatically attached when loading `maraca`.

# maraca 0.5.1

## Bug fixes

- Small bug fix to account for changes in HCE objects created in newest
  version of HCE package

# maraca 0.5.0

## New features

- `print()` function for maraca objects that summarizes key information.
- New `validate_maraca()` function that extracts key information from a maraca
  plot object. This can be used to validate the plot against independently coded
  versions (for example using a different programming language).
- The `maraca()` function now requires an input for the parameter
  `fixed_followup_days`. Note that there can be no observed events in the
  data after the follow-up time specified.

## Bug fixes

- Problem with jumps in the cumulative distribution functions fixed. As part
  of this fix, the package does not depend on the survival package anymore.

## Dependency change

- `maraca` does no longer depend on the `gridExtra` package.

## Discontinued features

- The `plot_tte_components()` function for plotting the individual time-to-event
  outcomes was removed from the package since it did not prove to be overly
  useful.
- The `plot_tte_composite()` was removed for now since the package cannot correctly
  calculate the composite version of looking at multiple time-to-event endpoints when
  patients have multiple events.

# maraca 0.4.0

## New features

- Added barriers to check validity of input parameters to plot.hce
- Plotting from single dataset
- Added documentation to plot.hce
- plotting for hce objects is implemented.
- Fixed plot type for violin and colors
- Updated to use the package hce.

# maraca 0.3.3

- First public release
