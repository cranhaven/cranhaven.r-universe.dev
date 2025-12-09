# isotracer 1.1.8 (2025-03-07)

## Minor fix

- Remove an unexpected dependency on the `lubridate` package when loading the isotracer dataset `aquarium_run`.

# isotracer 1.1.7 (2024-11-05)

## Minor fix

- Relax some thresholds used when performing numerical comparisons in the test suite to avoid spurious test failures.

# isotracer 1.1.6 (2024-05-14)

## Bug fixes

- Fix bug with how split compartments were handled when calculating steady state sizes. The issue was in the (internal) function `calculate_steady_state_one_row()` and affected the (exported) functions `tidy_steady_states()`, `tidy_flows()`, and `calculate_steady_state()`.
- Fix bug in `tidy_flows()` which was present when using `steady_state = TRUE` and pulses were present in the network model. Now running `tidy_flows(..., steady_state = TRUE)` automatically ignores any pulse that might be present in the network model.

## Improvements

- Add function `prop2delta()`.
- Check arguments to `delta2prop()` more thoroughly and provide helpful error messages.
- Remove dependency on the tidyverse package in the "Suggests" field of `DESCRIPTION`.
- Add tests for `tidy_steady_states()` and `tidy_flows()`.

# isotracer 1.1.5 (2023-09-21)

## Ensure package compatibility with future versions of rstan

- The array syntax used until now in the Stan models shipped with isotracer has been deprecated with the release of rstan 2.26 on CRAN. The Stan code included in isotracer now uses the new array syntax for compatibility with future versions of rstan, thanks to a merge request from Andrew Johnson.

# isotracer 1.1.4 (2023-03-20)

## Improved package compatibility with future versions of rstan

- The package installation/configuration is now delegated to rstantools, thanks to a merge request from Andrew Johnson. This should make the package compatible with future versions of rstan.

## Bug fixes

In the code:

- Fix bug in `sample_from()` (when `error.draws` was > 1, a mistake in the way sampled sizes and proportions were joined together resulted in too many rows in the returned table).
- Fix bug in `stanfit_to_named_mcmclist()` which was causing problems when running MCMC sampling with thinning.
- Fix bug in `quick_sankey()`.
- Fix minor bug in the handling of colors by `traceplot()`.

In the documentation:

- Fix the vignette "How to simulate experiments" (priors for two models were missing, which prevented the vignette code from running).

# isotracer 1.1.3 (2022-03-25)

## Minor change for resubmission to CRAN

- We were warned that one of the isotracer calls to a tidygraph function was triggering a fatal error when checking the isotracer package with the environmental variable "_R_CHECK_LENGTH_1_LOGIC2_" set to "abort,verbose". We modified one of isotracer's functions to avoid using the tidygraph code path that was triggering this error.

(This error message was related to the NEWS for R-devel: "Calling && or || with either argument of length greater than one now gives a warning (which it is intended will become an error)".)

# isotracer 1.1.2 (2022-02-16)

## Minor changes for resubmission to CRAN

- Add a random seed before some stochastic tests to ensure that the tests do not fail randomly from time to time.
- Drop dependency on rstantools for package users.

# isotracer 1.1.0 (2021-12-19)

## Major changes

- Implement a Stan model using matrix exponentials instead of the forward Euler method to solve the ODEs involved in a network model.
- Add a `method` argument to `run_mcmc()` to choose between `matrix_exp` (the default) and `euler` solvers.
- Drop the use of default priors (users must now explicitly set the priors). The documentation was also updated to provide more guidance about prior choice.
- Implement exponential and gamma priors.
- Change all names for prior functions from `*()` to `*_p()` to avoid collision with other R functions.
- Change the default output of `params()` to a tibble, for consistency with `priors()`.

## Other improvements

- Add a `euler_control` argument to `run_mcmc()` to be able to tune the fixed time step parameter of the Euler solver.
- Add a function `stanfit_to_named_mcmclist()` for easier manipulation of the output of `run_mcmc()` when a stanfit object is returned with `stanfit = TRUE`.
- Add `set_priors()` as a synonym for `set_prior()`.
- `set_priors()` now also accepts a tibble an as input to make copying priors across models easier.
- Add an `available_priors()` function to list all the priors implemented in
  the package.
- Improve `prop_family()` and add `size_family()` (to easily check the meaning of `eta` and `zeta` parameters).
- Add a `lalaja` dataset to the package so that the Trinidadian case study based on Collins et al. (2016) can be reproduced more easily.

## Bug fixes

- Fix a bug occurring when non-default `dt` or `grid_size` arguments were used for the Euler method.

# isotracer 1.0.4 (2021-09-24)

## Modifications for resubmission to CRAN

- Make re-export of functions from other packages cleaner.
- Improve documentation by adding some missing `\value` tags and removing examples using `:::`.

# isotracer 1.0.3 (2021-09-14)

## Modifications for resubmission to CRAN

- Fix issues about examples in the documentation (commented lines, `dontrun{}` vs `donttest{}`, examples for unexported functions).
- Add `@return` roxygen tags for all exported functions.
- Plotting functions don't manipulate `.Random.seed` directly anymore.

## Change in the handling of multiple cores for parallel runs

- The number of cores to use for parallel computation can now be set globally for the package by calling e.g. `options(mc.cores = 4)`.
- The code checking the number of cores to use also checks for the `_R_CHECK_LIMIT_CORES_` environmental variable to limit the number of cores to 2 when running on CRAN servers.

# isotracer 1.0.2 (2021-08-25)

## Modifications for resubmission to CRAN

- Fix issue about invalid file URIs in documentation.
- Reduce the size of the package tarball to 5 MB by:
    - making some plots in the vignettes smaller
    - ignoring rds files from `vignettes/` when building the tarball
    - not including the case studies based on McRoy (1970) and Li (2017) datasets in the CRAN package (but those case studies are still available on the pkgdown website).

## Change in the vignettes building approach

- Vignettes are now pre-compiled locally into dummy `Rmd` files to avoid issues with long run times and with the usage of multiple cores when performing R CMD checks.

# isotracer 1.0.1 (2021-08-15)

First version submitted to CRAN.

## Improvements

- Improve documentation of `trini_mod` dataset.
- Remove internal functions from documentation index.

# isotracer 1.0.0 (2021-08-15)

## Improvements

- Fix export of `select` generic so that it can be used without having to load `dplyr` first.
- Various minor fixes to address issues raised by R CMD check.

## Documentation

- Update Reference section.
- Remove vignettes containing development notes.
- Add citation information.

# isotracer 0.7.6 (2021-08-01)

## Improvements

- Add a subset method for `networkModelStanfit` objects.
- Minor changes in `prep-data/` scripts.

## Documentation

- Polish case studies vignettes and fix some typos.
- Create an hex sticker.

# isotracer 0.7.5 (2020-10-21)

## New features

- Add functions to draw Sankey plots.
- Add a `ggtopo()` function to plot topologies.
- Add a `ggflows()` function to visualize flows.
- Implement radioactive tracers.
- Implement more distributions to model observed proportions of marked material (gamma, normal parameterized by cv, normal parameterized by sd and beta).
- Implement compartment-specific values for `zeta` (the parameter for variation in predicted compartment sizes).

## Improvements

- Add a `c()` method for `mcmc.list` objects.
- Simplify the interface to define topologies.
- Speed up `tidy_trajectories()`, `tidy_flows()` and `predict.networkModel()`.
- Improve handling of constant parameters.

## Documentation

- Complete vignettes for the three selected case studies (nitrogen in Trinidadian streams from Collins 2016, phosphate uptake by eelgrass from McRoy 1970 and protein turnover in *Arabidopsis* from Li 2017).
- Polish all vignettes.

## Fixes/other changes

- Various bug fixes.
- Remove all uses of `furrr::future_map()`.

# isotracer 0.7.4 (2020-05-21)

## Improvements

- Include a simple aquarium model as a package dataset.
- Add functions `tidy_data()`, `tidy_posterior_predict()` and `tidy_dpp()` for easier posterior predictions.
- Improve colors in `mcmc_heatmap()`.

## Documentation

- All vignettes are now complete.
- New vignettes include "Units and priors", "MCMC output format", "Posterior predictive checks", "Calculating derived parameters", "How to simulate experiments", "Parameter identifiability".

## Fixes

- Fix issue so that `filter()` methods for isotracer S3 classes are correctly exported and do not depend on `dplyr` being attached first.
- Fix R CMD check issues so that check now passes on Linux, Windows and Mac (as tested with the **rhub** package).
- Use seeds for random number generation (including for Stan runs) to make vignette cached runs reproducible.

# isotracer 0.7.3 (2020-05-13)

## Features

- Improve plotting function (plot single trajectories and Greek letters in labels).

## Others

- Add (quite strict) version requirements for dependencies.
- Fix bug in `set_topo()` related to dependencies update.
- Improve test coverage.
- Use caching for long calculations in vignettes.

# isotracer 0.7.2 (2020-03-13)

## Interface changes

- Implement split compartments in the new interface.
- Add new prior: constant parameter (i.e. fixed parameter value).
- Add a `select()` method for `mcmc.list` objects.
- Improve plotting for predicted trajectories.
- Improve printing of topology.
- Polish the function names in the user interface.

## Documentation

- All tutorials have been updated to work with the latest interface.
- Vignette for Trinidad case study almost complete.

## Source improvements

- Major clean-up of the source files (remove old code, export only user-facing functions).
- Clean-up `tests/` folder and re-write tests from scratch.
- Address most issues reported by `R CMD check`.

# isotracer 0.7.1 (2019-12-12)

## Major changes

- The interface was entirely redesigned from scratch for better usability.
- This was done in parallel with a lot of code clean-up, with the aim of having
  simpler and more easily extensible code that would carry less constraints
  from its history of being originally developed for the Trinidadian streams
  case study.
- The code is moving away from using R6 classes, which makes things simpler
  from a developer perspective (no need for careful deep-cloning to avoid
  unwanted references pointing to the same object in memory).
- The tutorials describing the basic use of the package have been updated.
  
## Missing features

- Some features from the old implementation are still missing, such as split
  compartments.
- Some tutorials still need to be updated to work with the new interface.
- Tests and coverage have not been updated at all to work with the new interface.

# isotracer 0.6.2 (2019-09-06)

## Major improvements

- Implement the network model in Stan. Model fitting is faster and more
  efficient compared to the home-made MCMC sampler we used before.
- Implement adaptive dt selection (time step size) in the numerical solver
  used to project trajectories in the Stan model.
- Allow the user to specify customized parameter priors. Currently available
  priors are Uniform, half-Cauchy and (scaled) Beta.

## Minor improvements

- Add an option to use histograms instead of kernel density estimates when
  plotting MCMC traces.
- Improve the color palette and variable labels in `mcmc_heatmap()`.
- Add a `derived.mcmc.list` class to objects returned by Math generics for MCMC
  output (to automatically use `isotracer` plot method for derived parameters).
- Remove some unnecessary dependencies from DESCRIPTION.
- Reduce the output verbosity of the Makefile rules.
- Improve handling of time steps during numerical projection of the
  trajectories to make it more robust.

## Documentation and installation

- Update the vignettes to work with the Stan implementation.
- Improve vignettes aesthetics.
- Fix installation issues when using `devtools::install_gitlab()`.
- Fix the documentation for cloning the repository locally.
- (Fix/improve Rcpp code export for use with home-made sampler, but then made
  irrelevant by using Stan.)

## For developers

- Also implement trajectories calculation using matrix exponential (in the Stan model), but inefficient.
- Also implement trajectories calculation using built-in ODE solver (in the Stan model), but inefficient.

## Things to improve/fix for next version

- To fix: plotting the Sankey plot for estimated fluxes does not work yet with
  the new Stan implementation.
- To fix: R CMD Check was deactivated on the Gitlab repository to make the CI
  pipeline faster.
- To fix: profiling the model fitting does not make sense any more now that we
  are using Stan. Is there any other critical code to profile, or can we
  abandon profiling when running the CI pipeline?

# isotracer 0.6.1 (2019-02-25)

## Interface

- Add a `tidyOutput()` function to the core interface.

## Documentation

- Simplify installation instructions.
- Prune old vignettes.
- Add a profiling output (for developers).
- Use Runner serialization for caching of intermediate results during vignette
  building.

## Implementation

- Remove source code for previous implementation.
- Prune corresponding tests.
- Add serialization to Runner class to be able to save a Runner's parameter
  state and sampler state in a small file and restore them in another session.

# isotracer 0.6.0 (2019-02-22)

## New user interface

- Major change to the user interface to design a network model.
- Implements different types of inputs (drip, pulse, flow) (`run()` method only
  working for flow input for now).
- Implement `run()` method in pure R first for use as a reference when
  optimizing speed with C++.
- Implement half-life for tracers which can decay with time (radioactive
  isotopes).
- Implement split compartments.

## Tidifying the package

- Tidify datasets shipped with the package by adding functions `datasetTopo()`,
  `datasetAddition()` and `datasetObs()`.
- Tidify the way the experimental units (replicates) are stored in a network
  model.
- Add a `tidyChain()` function to get the MCMC run of a network model in a tidy
  format, including columns with parameter values and network trajectories.
- Similarly, add a `tidyFluxes()` function to get run-averaged fluxes and
  compartment sizes for each iteration.

## Visualization

- Add `$renderMovie()` method to `SingleNetwork` object to make an animation of
  tracer enrichment in a network.
- Improve `plot.topology()` method output.
- Create functions `plotEstimatedProp()`, `plotEstimatedSize()` and
  `plotSankey()` compatible with the new implementation.

## Documentation

- Update some tutorial vignettes to take into account the new interface (Quick
  start, Modelling a small foodweb, new vignette Post-run diagnostics and
  analyses, Handling replication, Model Selection and Derived Parameters).
- Remove tutorial about simulating datasets.

## Miscellaneous

- Improve test coverage.
- Make installation easier for new users by tracking package .rda files.

# isotracer 0.5.0 (2018-11-26)

No major changes to the package, but a lot of small fixes, minor
improvements and documentation additions.

## Documentation: choosing candidate datasets for case studies

- Select candidate datasets to be used for case studies.
- Prepare the dataset modified from McRoy & Barsdate 1970 (eelgrass).

## Preparing CRAN submission: R CMD check

- Fix/polish a lot of small things to avoid errors/warnings/notes from R CMD check.

## Other changes

- Add a formal R6 class `Proposal` to be used as a parent for all proposal
  classes.
- Implement colored text-mode plots, inspired by the `txtplot` package and
  using the `crayon` package for colored output in the terminal.

# isotracer 0.4.0 (2018-09-11)

## License change

- Change license from MIT to GPL-3

## User interface and documentation

- Improve the user interface to the core MCMC engine to better separate
  interface and implementation (the user does not have to access any R6 method
  or attribute directly).
- Create a new vignette to present the user interface to the core MCMC engine.

## Other changes

- Refactor slightly the Runner class for cleaner presentation of methods.
- Improve test coverage (by adding test cases and dropping some unused code).

# isotracer 0.3.0 (2018-09-04)

- The package, formerly known as `mcmcssm`, is renamed `isotracer`.
- An older, separate package containing the first implementation of the method
  was already called `isotracer` (this older package was not submitted to CRAN
  though - it was living on the maintainer computer and on GitLab). To
  distinguish between them, this other package has been renamed
  `isotracerPrototype`. This older package never reached above version 0.0.2
  under the `isotracer` name, so there is no possible confusion between the two
  packages when version number is taken into account.

# mcmcssm 0.2.0 (2018-09-03)

- Basic encapsulation is now available to use the core MCMC engine.
- Speed was improved for `FoodwebModel` MCMC run, in particular for models with
  split compartments.

# mcmcssm 0.1.0 (2018-08-22)

- This is the first version tagged in the repository.
