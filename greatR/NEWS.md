# greatR 2.0.0

* Added {patchwork} as a dependency.
* Updated {greatR} logo.
* Updated default `optimisation_method` in `register()` to be "lbfgsb" (LBFSG-B) instead of "nm" (Nelder-Mead).
* Added sample SOC1 `arabidopsis_SOC1_data.csv` and `brapa_SOC1_data.csv` extdata.
* Refactored `optimise_registration_parameters` argument in `register()` to `use_optimisation`.
* Updated `register()` to return object of S3 class `res_greatR`.
* Updated `calculate_distance()` to return object of S3 class `dist_greatR`.
* Refactored `summarise_registration()` as `summary.res_greatR()` S3 method.

## Improvements

* Deprecate use of `time_delta` variable in registration process.
* Added `fun_args` (a list of arguments used when calling the function) in `register()` results.
* Updated `summary.res_greatR()` to return `NA` instead of `[NA, NA]` when all genes are non-registered.
* Added `reg_params` (table containing distribution of registration parameters) to results list in `summary.res_greatR()` method.
* Simplified `calc_overlapping_percent()` calculation.
* Take into consideration `overlapping_percent` when applying manual registration.
* Updated logic of `calc_variance()` for data with no replicates to consider `expression_value`.
* Updated `get_stretch_search_space_limits()` and `get_shift_search_space_limits()` to exclude unexplorable regions in search space.
* Improved `calculate_distance()` and aux `get_timepoint_comb_*_data()` functions to eliminate column selection and renaming inside `lapply()` calls, reducing execution time by up to 25%.
* Added `type` ("registered" or "all") and `genes_list` arguments to `calculate_distance()` to filter genes.
* Added new unit tests.
* Updated unit tests, and added S3 class checks where apropriate.
* Updated vignettes and README diagrams and figures.
* Updated vignettes with additional examples, comments on arguments, and full coverage of all `plot()` methods.

## Bug fixes

* Fixed `get_shift_search_space_limits()` to adjust shift space limits accordingly to removal of `time_delta` variable (see 48c943cd).
* Fixed default `overlapping_percent = 0.5` (instead of 50) in `register_manually()`.
* Fixed `get_stretch_search_space_limits()` to correctly determine lower and upper limits when single stretch value is provided.
* Fixed issue in `get_shift_search_space_limits()` where range variables were not available when `calc_mode == "bound"`.

## New functions

* `bind_results()` auxiliary function to merge results from `register()`.
* `theme_greatR()` function and `greatR_palettes` list.
* `transform_input()` S3 generic to accept different types of input in `register()`.
* `plot.res_greatR()` S3 method to replace `plot_registration_results()`.
* `plot.dist_greatR()` S3 method to replace `plot_heatmap()`.
* `plot.summary.res_greatR()` S3 method inspired by `WVPlots::ScatterHistC()`.

# greatR 1.1.0

* Added {furrr} and {future} as dependencies.
* Added `num_cores` parameter to `register()` to allow users to run registration in parallel.
* Added `exp_sd` parameter to `register()` to allow users to manually set up experimental gene expression variance.
* Updated `scaling_method` parameter in `register()` and `scale_data()` to allow no scaling ("none", default), Z-score scaling ("z-score"), and min-max scaling ("min-max"), and updated unit tests accordingly.

## Improvements

* Updated `register()` to perform 3 sequential registrations when using Nelder-Mead, this improves the results of optimal stretch and shift parameters.
* Updated `calc_loglik()` to use `sigma_squared` in every time point in the sum.
* Updated `scaled_data()` and `preprocess_data()` to return `all_data` object only, instead of a `list()` containing `all_data`.
* Updated `compare_H1_and_H2()` to return `BIC_diff` column (`BIC_combined - BIC_separate`), instead of `BIC_combined` and `BIC_separate` on their own.
* Updated `explore_manual_search_space()` to use `BIC_diff` instead of `BIC_combined` to calculate `best_params` from `model_comparison` table.
* Updated `register()` to perform 3 sequential registrations when using Nelder-Mead, this improves the results of optimal stretch and shift parameters. This may be reverted by tweaking `neldermead()` parameters to ensure correct convergence.
* Added optional `stretch_init` and `shift_init` to `get_search_space_limits()`, and updated `optimise()` to allow for different `space_lims` calculation settings: automatic, given boundary box, and given initial coords (new).
* Removed unused `mean_data` calculation from `preprocess_data()` and argument from `scale_data()`.
* Moved "Will process N genes" message from `register()` to `preprocess_data()` after running `filter_*()` functions.
* Ensure `results_list$data` is arranged/ordered correctly in `register()`.
* Updated `get_H*_model_curves()` functions to ensure model curves are smooth.
* Updated `parse_gene_facets()` to display `BIC_diff` in facet strips.
* Added `plot_mean_data` parameter to `plot_registration_results()`.
* Updated `overlapping_percent` parameter in `register()` so it goes from 0 to 100 (it's later normalised in the function to avoid breakages down the line).
* Added `scaling_method` as an attribute in `data` results from `register()`, this is used in `plot_registration_results()` to build the y-axis label according the the scaling method used.
* Updated `brapa_arabidopsis_registration.rds` file with new pipeline results.
* Split `get_search_space_limits()` into separate aux functions for stretch and shift, which allows more stretch and shift input combinations.
* Updated `validate_params(..., registration_type = "optimisation")` to allow more stretch and shift input combinations.

## Bug fixes

* Improved `get_timepoint_comb_original_data()` and `get_timepoint_comb_registered_data()` to perform `cross_join()` on a single `gene_id` at a time using `lapply()`, this fixes "Error: vector memory exhausted (limit reached?)" error.
* Updated `match_names()` to do double `setdiff()` to ensure name matching is done two ways, and updated corresponding unit test.

## New functions

* `filter_incomplete_accession_pairs()` to filter out genes that are missing one accession.
* `calc_variance()` to preprocess data variance inside `preprocess_data()` instead of `calc_loglik()`.
* Aux `register_single_gene_*()` functions inside `register()` to simplify and generalise the pipeline for parallel registration.

# greatR 1.0.0

* Rewrote registration pipeline from scratch, deprecating unnecessary, and redundant auxiliary functions.
* Added L-BFGS-B and Nelder-Mead (now default) optimisation methods to {greatR}.
* Switched to manual calculation of log likelihood via `calc_loglik()` instead of `stats::logLik()`.
* Reduced computation time up to 1000 times, (x30 speed-up from package rewrite, and x35 speed-up from switching default optimisation method).
* Removed {dplyr}, {magrittr}, {purrr}, {rlang}, and {stringr} as package dependencies.
* Added {neldermead} as a package dependency.
* Updated list of exported functions:
  * `register()`
  * `summarise_registration()`
  * `get_approximate_stretch()`
  * `plot_registration_results()`
  * `plot_heatmap()`
  * `calculate_distance()`

## Improvements

* Simplified parameters of main `register()` function, and added `scaling_method`.
* Simplified structure of output object of `register()`.
* Simplified parameters of `summarise_registration()`, `plot_registration_results()`, `plot_heatmap()`, `calculate_distance()` to simply require `results` object from `register()`, vastly simplifying usage.
* Improved messages, errors, and progress indicators with {cli}.
* Added correct pluralisation in {cli} messages.
* Rewrote unit tests to use {data.table} exclusively for data manipulation.
* Added unit tests for `calc_loglik_H1()`, `calc_loglik_H2()`, `calc_overlapping_percent()`, `calculate_distance()`, `cross_join()`, `get_search_space_limits_from_params()`, `get_search_space_limits()`, `objective_fun()`, `optimise()`, `plot_heatmap()`, `plot_registration_results()`, `preprocess_data()`, `register_manually()`, `register()`, `summary_registration()`, `validate_params()`.

## Bug fixes

* Fixed `match_names()` call when validating accession names in `register()`
* Fixed use of deprecated `aes_string()` by parsing `timepoint_var` using `!!ggplot2::sym()` call.
* Fixed `preds` left join in `plot_registration_results()`.
* Fixed issue in `plot_registration_results()` not working when all genes are unregistered with `type = "registered"`.
* Fixed calculation of `time_delta` in `preprocess_data()` to ensure it's grouped by `gene_id` and `accession` (not just `accession`).

# greatR 0.2.0

* Added Alex Calderwood as package co-author.
* Added vignette for optimisation process.
* Refactored `num_shifts` and `shift_extreme` parameters by simplified `shifts` parameter.

## Improvements

* Improved default parameter values in exported functions.
* Added {optimization}, {purrr} as package dependencies.
* Removed {cowplot}, {ggpubr}, {ggrepel}, {Rtsne}, and {viridis} as package dependencies.
* Cleaned up {cli} messages.
* Removed legacy AIC references, as it is no longer used.
* Updated `calculate_between_sample_distance()` to use `registration_results` as primary parameter instead of `mean_df`, `mean_df_sc`, and `imputed_mean_df`.
* Added warning if there is no comparable time points found using users' pre-defined parameters.
* Refactored `optimise_shift_extreme` as `maintain_min_num_overlapping_points`, properly defined and corrected the boundary box if number overlapping points whether needed to be maintained or not.

## Bug fixes

* Check that input accessions exist in the input data in `get_approximate_stretch()`.
* Manually create time point sorting levels for `x_sample` and `y_sample` columns according in `plot_heatmap()`.
* Properly handle `-` character in accession names in `plot_heatmap()` so that time points are parsed correctly.

## New features

* Added optional parameter optimisation process using Simulated Annealing through `optimise_registration_params()`.

## New functions

* `preprocess_data()` to simplify `scale_and_register_data()` code and reuse logic elsewhere.
* `get_best_stretch_and_shift_simplified()`.
* `get_BIC_from_registering_data()`.
* `get_boundary_box()`.
* `optimise_registration_params_single_gene()`.
* `optimise_registration_params()` as wrapper of `optimise_registration_params_single_gene()` for multiple genes.
* `get_best_stretch_and_shift_after_optimisation()`.

# greatR 0.1.0

* Initial release.
* Added a `NEWS.md` file to track changes to the package.
