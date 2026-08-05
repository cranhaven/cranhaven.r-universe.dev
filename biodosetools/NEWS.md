# biodosetools 3.7.1

* New module for criticality accidents dose estimation.
* New module for calculating chacaracteristic limits.
* New module for interlaboratory comparisons.

## Improvements
* Now multiple dose calculations can be done at the same time.
* .xlsx save.
* Error message when missing fields
* Error message when there are no C2 cells for partial body assessment.
* Separated micronucleus assay.


# biodosetools 3.6.1.1

* Updated citation details for {biodosetools} after publication in Int J Radiat Biol (doi:10.1080/09553002.2023.2176564).
* Improvements in `deploy-shiny.yml` CI workflow.
* Added `dev/shiny-redirect` shiny app to redirect `biodosetools-v3` at shinyapps.io to version hosted by Bundesamt für Strahlenschutz.

# biodosetools 3.6.1

* Removed {pkgload} and {htmltools} as package dependencies.
* Server modules now use `moduleServer()`, as recommended in {shiny} >= 1.5.0 (see https://shiny.rstudio.com/articles/modules.html).
* Updated `golem_utils_ui.R` and related unit tests to match {golem} 0.3.x (see https://github.com/ThinkR-open/golem/commit/61b9063f65006bc9b15dd2f1e535466c45d25424).
* Updated CITATION file.
* Updated vignettes.

## Improvements

* Updated LQ and L formulas to use lambda instead of Y for yield in `parse_model_formula()`, `list_fitting_formulas()`, and help dialogues.
* Removed unnecessary {base} namespace in `solve()` call.
* Removed legacy `gardner_confidence_table` data (deprecated in [585e7b4](https://github.com/biodosetools-team/biodosetools/commit/585e7b4b6f42e66965e3b0f6a271dbaea7b7cf28)).
* Updated `get_cov_ZIP_ML()` to match expressions on manuscript, introducing minor calculation optimisation.
* Improved `fit_maxlik_method()` by removing unused local variables and redundant calls.
* Switched `message()` to `cli::cli_alert_warning()` in `fit()` function when switching from `glm()` to ML optimization.
* Updated `match_names()` to return input `x`, akin to `match.arg()` from {base}.
* Updated default values for assessment and whole-body error method selection in `mod_estimation_micro_ui()`.
* Added `mean` and `std_err` column renaming to `calculate_aberr_table()` when `type = "case"` depending on new `aberr_module` parameter.
* Added `aberr_module` validation using `match.arg()` in `*_aberr_table()`, `estimate_*()`, `prepare_maxlik_count_data()`, `fit*()` functions.
* Updated unit tests for `names_from_model_formula()` and `parse_model_formula()`.
* Updated `app_config.R` and `test-golem-recommended.R` unit tests.
* Added unit test for `load_rmd_report()`.
* Added basic `testServer()` unit tests for all `mod_*_server()` modules.

## UI Improvements

* Updated "Irradiation conditions" boxes so they are no longer collapsed by default in `mod_fitting_*_ui.R` modules.

## Bug fixes

* Return case data `as_tibble()` in `calculate_aberr_table()` for consistency with count data.
* Minor formatting fix in `cli::cli_alert_info()` call.
* Fixed `pi_estimate` value in `est_metaphases_frac` data frame in `estimate_partial_body_dolphin()`, as pointed out by Gaëtan.
* Fixed issue with `match_names()` not correctly stopping, and added unit tests for it.
* Fixed misconstructed column selection in `calculate_aberr_table()` when `type = "case"`.
* Added `est_metaphases_frac` data frame to explicitly return `f (1 - pi_est)` in `estimate_partial_body_dolphin()` (closes #29).
* Updated deprecated `.data` calls inside {tidyselect} selections (see https://www.tidyverse.org/blog/2022/10/tidyselect-1-2-0/).

## Deprecations

* Removed unused files in `data-raw`.
* Deprecated `model_formula` with no intercept in `parse_model_formula()`, `names_from_model_formula()`, `prepare_maxlik_count_data()`, `fit_maxlik_method()`, and `mod_estimation_fit_curve_hot_server()`.
* Removed `golem_utils_server.R`.
* Purged `calculate_decision_threshold*()` functions and mentions in UI and server modules, as this will be rewritten from scratch.
* Removed `calculate_decision_threshold` from `_pkgdown.yml`.

# biodosetools 3.6.0

* Major rewrite of `plot_estimated_dose_curve()` function.
* Delta method calculations are now all done via `msm::deltamethod()`.
* Added progress and alert notifications to UI.
* Added {markdown} as suggested dependency.
* Added {cli} as dependency.
* Tweaked references section in {pkgdown} site to only include exported functions organised by topic/usage.
* Use consistently genomic conversion factor or `genome_factor` across all functions, help dialogues, and reports.
* Added R-CMD check GitHub Action.
* Added vignettes.

## New features

* Return `conf_int` in `estimate_*()` family of functions.
* Parse `est_full_doses$type` in `plot_estimated_dose_curve()` function so unused assessments are not shown in legend.
* Added `match_names()` function to match est_doses list names when calling `plot_estimated_dose_curve()`.
* Updated `plot_estimated_dose_curve()` to automatically build `est_full_doses` object from list of dose estimation results, added `parse_conf_int_text()` auxiliary function.

## Bug fixes

* Correct dispersion index value on `estimate_whole_body_delta()` when there's no aberrations.
* Fixed issue in missing count data in DOCX fitting report.
* Removed double calculation of `cov_est` in `estimate_partial_dolphin()`.
* Wrapped mixed Poisson model dose estimation in `try()` to ensure convergence (up to 5 tries).
* Mixed Poisson model `estimate_hetero_mixed_poisson()` uses exact delta methods for dose and fraction irradiated, as described in paper by Pujol et al. (2016) <https://doi.org/10.1667/RR14145.1>.
* Fixed `gamma` and `gamma_error` parsing when calling `estimate_hetero_mixed_poisson()` in `mod_estimation_results_server()`.
* Wrap unicode characters (`\uxxxx`) in `rlang::as_utf8_character()` to avoid "unable to translate to native encoding" warning on Windows.

## New functions

* `get_deltamethod_std_err()`: auxiliary function to wrap all `msm::deltamethod()` calls.

## Improvements

* Fixed order of output case data columns in `calculate_aberr_table()` so they match the final output in Shiny.
* Renamed `count-data-IAEA.csv` to `count-data-barquinero-1995.csv` from Barquinero et al. (1995) <https://doi.org/10.1016/0027-5107(94)00150-4>.
* Added `count-data-rodriguez-2004.csv` for total translocations from Rodriguez et al. (2004) <https://doi.org/10.1667/RR3198>.
* Updated translocations fitting unit tests to use data from Rodriguez et al. (2004) <https://doi.org/10.1667/RR3198>.
* Rewrote `calculate_aberr_power()` to remove {purrr} dependency and reduce computation time by 4.
* Replaced `message()` calls with appropriate `cli::cli_*()` calls.
* Split quasi-Poisson from automatic fitting in `fit_glm_method()`, as it is otherwise confusing.
* Updated `\sigma` to `\hat{\sigma}` in `fix_count_data_names()` and respective tests.
* Updated unit tests to test individual values both in dose-effect fitting and dose estimation.
* Use `*-fitting-results-*.rds` instead of `*-fitting-data-*.rds` when exporting RDS objects in `mod_fitting_results_server()`.

## UI Improvements

* Added new `side-widget-*` CSS classes to better style file download/format buttons.
* Added `widget_sep_vert()` function.
* Use new `sep-widget_*` CSS classes in fitting UI modules.
* Updated report help modals text.
* Using new `side-widget-*` CSS classes in dose estimation UI modules.
* Added progress notifications to calculations via `shiny::Progress`.
* Updated `_pkgdown.yml` and simplified `extra.css` thanks to Bootstrap 5 support in {pkgdown} 2.0.0.


# biodosetools 3.5.0 (2021-05-26)

* Added unit tests for to check code coverage and to validate that the code performs as expected.
* Major revision of the fitting and dose estimation pipelines, introducing new auxiliary functions to reduce duplicated code.
* Major simplification of report rendering process; abandoned HTML output.
* Added custom {pkgdown} theme for more consistent branding.
* Added preliminary citation to README.
* Moved packages required only for reports to `Suggests` field in `DESCRIPTION` file.

## New features

* Added irradiation conditions input to dose-effect fitting modules.
* Reports include conditional formatting of `u`-value and other formatting refinements.
* Added `biodosetools_version` element to Shiny App's exported `*-fitting-data-YYY-MM-DD.rds` files.
* `fit()` function allows to optionally select `"glm"` or `"maxlik"` algorithms. If `"glm"` if selected, the original `tryCatch()` routine will be executed.

## Bug fixes

* Fixed unexported `calculate_aberr_var()` function.
* Stopped using weights in fitting algorithms (`glm` and `glm.nb`). Fixes #20, addresses part of #14 as well.
* Fixed aggregated count data column parsing in `fix_count_data_names()` function.
* Fixed case data parsing issue when `.csv` file is not perfectly formatted.
* Fixed issue with manual fitting curve input (fixes #23).
* Fixed bug in `calculate_yield_infimum()` where infima were being calculated for yield estimate only (fixes #26).
* Fixed wrong calculation of aberrations `X` for `Ck` when `k>=10` (fixes #27).
* Fixed `fix_count_data_names()` to properly correct `Ck>=C10` (related to issue #27).
* Added "where" to global variables (see https://github.com/r-lib/tidyselect/issues/201).

## New functions

* `inner_column()`, to fix padding of boxes inside columns.
* `widget_sep()`, to insert `div(class = "widget-sep", br())` calls.
* `widget_sep_vert()` to insert `div(style = "height: 8px;")` calls.
* `names_from_model_formula()`, to parse `rhandsontable()` Unicode row and column coefficient names.
* `parse_model_formula()` to get raw and TeX formulae from `model_formula`.
* `calculate_trans_rate_sigurdson()` and `calculate_trans_rate_manual()` to calculate translocation rates.
* `calculate_aberr_table()` wrapper, to calculate aberration tables for count and cases data.
* `init_aberr_table()`, to initialise aberration distribution tables in fitting and estimation server modules.
* `list_fitting_formulas()`, to replace global `global_fitting_formulas` object.
* `generalise_fit_coeffs()` and `generalise_fit_var_cov_mat()`, used internally in estimation functions instead of using `general_fit_*` as parameters.

## Function refactoring

* Refactored `get_decision_threshold()` function to `calculate_decision_threshold()`, and added `calculate_decision_threshold_table()` wrapper function.
* Refactored `calculate_decision_threshold_*()` functions to remove `input` argument.
* Refactored `get_*()` translocation functions to `calculate_*()` for better consistency.
* Refactored `get_*_dose_curve()` functions to `plot_*_dose_curve()` for more clarity.
* Refactored `get_model_statistics()` to `calculate_model_stats()`.
* Refactored `get_fit_*()` functions to `fit()` and `fit_*_method()`.

## Improvements

* Added Oliveira citation on `get_fit_maxlik_method()` function.
* Fixed Gaëtan's name order in citation and contributors list.
* Updated README and About body text.
* Renamed source R files for better naming consistency.
* Changed structure of modules to `mod_<calc_type>_<aberration>_*()`.
* Generalised `get_model_statistics()` function so that local `get_model_statistics()` definition could be removed from `mod_estimate_fit_curve_server()` module.
* Multiple (18 files) `<aberration>-<module>-<format>.Rmd` report templates have been merged into `<module>-<format>.Rmd` (4 files).
* Use `correct_negative_vals()` to ensure correct dose estimation when `X < Xc` in translocations assay.
* Replaced all `*_at()` and `*_if()` occurrences by their {dplyr} 1.0.0 equivalents.

## UI Improvements

* Replaced `column(width = X)` calls by `col_X()` in UI modules.
* Added `tabitem-container` class to `tabItem()` page containers for fixed `max-width` while keeping responsive UI.
* Added `col-inner-textinput-*` CSS classes for Irradiation conditions `textInput()` widgets' containers.
* Added `sep-widget-download` and `sep-widget-format` CSS classes to unify download and format select buttons into a single widget.


# biodosetools 3.4.0 (2020-10-11)

* Removed all non-ASCII characters.
* Major overhaul of reports.
* Added "Report bug" button on home screen and cleaned up `dashboard_*()` code.
* Removed dependencies on {stringr} package.
* Initial `utils::citation()` support via CITATION file.
* Minimal version of R bumped to 3.5.0.
* Minimal version of {dplyr} bumped to 1.0.0.

## Bug fixes

* Replaced `C`, `α`, `β` variables (and derivatives) by `coeff_C`, `coeff_alpha`, `coeff_beta`.
* Fixed error in genome_fraction parsing for translocations in `estimate_partial_dolphin()`.
* Fixed count/case data standard error column names replacements.
* Fixed minipage issue when using more than 12 columns in {knitr} tables with column name replacements, by using `format = "latex", escape = FALSE` parameters in `kable()` call.
* Suppress YAML warning when using `!expr` in PDF reports. See https://github.com/rstudio/rstudio/issues/7545.

## New functions

* `fix_coeff_names()`, to fix coefficient names in reports.
* `fix_count_data_names()`, to fix count/case data column names in PDF reports.
* `to_title()`, to replace `stringr::str_to_title()` using base R.

## Reports improvements

* New PDF reports, which are now the default output format.
* Improved HTML reports styling.
* Added explicit M-FISH usage in translocation reports.
* Added assay name in report titles.


# biodosetools 3.3.1 (2020-10-07)

## Bug fixes

* Count and case data calculations are performed using `calculate_aberr_*()` functions. Fixes #8.
* Fixed missing {dplyr} namespace in `n()` function call.
* Fixed mismatched use of `awesomeCheckbox()` and `switchInput()` in confounders input.

## New functions

* `calculate_aberr_power()`, which supersedes internal `aberr_calc()` function in server modules.
* `calculate_aberr_mean()`.
* `calculate_aberr_var()`.
* `calculate_aberr_disp_index()`.
* `calculate_aberr_u_value()`.


# biodosetools 3.3.0 (2020-07-27)

Initial migration of the app into a {golem} R package. This means Biodose Tools is also available as a regular R package in case the user wants to build their own R scripts. This also reduced lots of code redundancies, and made the code a lot more robust.

## New features

* App is now a {golem} R package.
* Migrated UI from {shinydashboard} to {bs4Dash}.
* Modals are now built with {bsplus} instead of {shinyBS}.
* Functions and internal data are documented using {roxygen2}.
* Added {pkgdown} support to build {biodosetools}'s website automatically.
* All custom theming is done through SASS and CSS instead of R code injection.

## New functions

* New `include_help()` for help dialogues
* New `help_modal_button()` function to help build modal trigger buttons on boxes' titles.
* New `load_rmd_report()` function to use in server `downloadHandler()`.
* Moved `get_genome_fraction()` as its own exported function.

## Fixes

* Fixed #7. Overestimate error of lambda when dispersion is smaller than 1.
* Fixed missing icon on sidebar.
* Fixes in collapse button paddings.
* Fixed `tabBox` parameters.
* Using `rlang::.data$` to avoid "no visible binding for global variable" warnings.
* Fixed `bsplus::bs_modal()` calls in `output$estimate_results_ui`.
* Removed input as parameter in calculation functions.
* Fixed style of help button on `box`/`tabBox` headers.
* Updated old "biodosimetry-uab" URLs to "biodosetools-team".

## Improvements

* Stop exporting auxiliary `include*()` functions used on the app only.
* Moved `widget_label()` to widgets.R.
* Refactored `inner_column()` function.
* Refactored UI modules names.
* Refactored server functions and renamed R scripts.
* Updated reports.
* Using `load_rmd_report()` on `mod_estimate_results_server()` module.
* Added DNA Content Fractions of Human Chromosomes (IHGSC) data.
* Updated `dna_content_fractions` call on `mod_trans_fraction_to_full_genome_server()` module.
* Added support for `tabBox` on `help_modal_button()` widget.
* Added indentation to all module `rhandsontable()` calls.
* Stop exporting `get_decision_threshold()` function temporarily.
* "Stains color options" boxes don't use `inner_column()` anymore.
* Updated `NEWS.md` headers so they can be parsed by {pkgdown}.

## UI Improvements

* Removed `experiment_select` `selectInput()` from sidebar. Every assay is listed on the "Aberration assays" `sidebarMenu()`.
* Loading all UI modules in `dashboard_body()`.
* Navbar now uses `rightUi` and `leftUi` parameters, as well as `skin = "light"`.
* Switched from `shinyBS::bsButton()` to `shiny::actionButton()` for calling modal dialogues.
* New CSS style sheet for {shinydashboard}.
* Deleted old CSS files for {bs4Dash}.
* New fonts CSS stylesheet.
* New CSS stylesheets for fixes of existing classes and definition of custom classes.
* Added `hot-improved` CSS class for `rHandsontableOutput()`.
* Moved custom widgets classes from `theming.R` to `custom.css`.
* Tweaks in `box`, `awesome-checkbox`, and sidebar submenus CSS styles.
* Replaced `theme_button_status()` function by proper SASS to CSS compilation.
* Finished SASS box header colors.
* Using SASS modules to reconstruct `biodose_style.css`.
* Moved treeview-menu style to `sidebar.scss`.
* Added checkboxes SASS style.
* Added missing `col-inner-*` CSS classes.
* Added app version on sidebar footer with custom `sticky-footer` CSS class.
* Switched to a single stylesheet, being everything built by SASS modules.
* Added custom style to `radiobuttons`.


# biodosetools 3.2.1 (2020-02-13)

## Bug fixes

* Added required {pander} package to generate DOCX reports.


# biodosetools 3.2.0 (2020-02-11)

All calculations functions previously provided in `inst/app/calcs` have been made proper functions on the package.

## New functions

* `calculate_yield()` new wrapper of `yield_fun()`, `R_factor()`, and `yield_error_fun()`.
* `calculate_yield_infimum()` function to calculate infima of yields given a curve.
* `project_yield()` merged version of the `project_yield_estimate()`, `project_yield_lower()`, and `project_yield_upper()` functions.

## "New" functions

* `get_decision_threshold()`.
* `get_fit_dose_curve()`.
* `get_fit_glm_method()`.
* `get_fit_maxlik_method()`.
* `get_fit_results()`.
* `get_model_statistics()`.
* `prepare_maxlik_count_data()`.
* `AIC_from_data()`.
* `correct_boundary()`.
* `correct_conf_int()`.
* `correct_negative_vals()`.
* `correct_yield()`.
* `get_estimated_dose_curve()`.
* `protracted_g_function()`.
* `R_factor()`.
* `yield_error_fun()`.
* `yield_fun()`.
* `estimate_hetero()`.
* `estimate_partial_dolphin()`.
* `estimate_whole_body_delta()`.
* `estimate_whole_body()`.


# biodosetools 3.1.0 (2019-10-26)

Unofficial release (wasn't changed on `DESCRIPTION` file). This includes some of the changes discussed with David in Stockholm (ERPW19).

## New features

* Made var-cov matrix optional on dose estimation inputs.
* Fixed calculations to make var-cov matrix optional on dose estimation inputs.

## Fixes

* Added additional package dependencies to `DESCRIPTION` file.
* Following the ISO, renamed detection limits to decision threshold.

## Changes

* Disabled decision thresholds (for now).
* Hide AIC as a relative quality statistic of the dose estimation.
* Added mean and variance to count data tables in fitting module.


# biodosetools 3.0.0 (2019-10-12)

The app is now available as an R package on GitHub (not submitted to CRAN yet).

## New functions

* `runApp()` for launching Biodose Tools.
* `%>%` imported from {magrittr}.

## Fixes

* Fixed variance calculation for count data.
* Provide fallback method for NB2 when using constraint-maxlik-optimization.


# biodosetools 2.1.0 (2019-07-27)

## New features

* Initial implementation of micronuclei, adding support in UI and server functions.
* Implemented negative binomial fitting calculation for micronuclei count data.
* Initial detection limit implementation for translocations.
* Added DOCX support for dicentrics and translocations.

## New functions

* New `bs4MyTabPanel()` widget to remove unnecessary padding on `tabCards`' panels.

## Fixes

* Fixed `colwidth`s for detection limits.
* Fixed unnecessary recalculation of `num_cols` for chromosome tables in translocations modules.
* New subdirectory structure for help dialogues.
* Added buttons and example tables in help dialogues.
* Updated documentation link.

## Improvements

* Added `button_fit` dependency for detection limits calculation.
* Updated fitting reports (new hot structures & dynamic widths).
* Moved calculation functions from fitting and estimation modules to their own files in a new `calcs` directory.
* Translocations reports are complete now.

## Changes

* Disable sourcing `translations.R` for now (until {shiny-i18n} is implemented).


# biodosetools 2.0.0 (2019-07-17)

## New features

* Added manual input of translocations name.
* Implemented dynamic calculation of hot width for counts, cases, and chromosome data tables.
* Implemented manual input of translocation frequency to be subtracted from the observed yield.
* Started detection limit calculation in fitting module.
* Implemented Delta method for whole-body estimation.
* Added calculation of standard error of `Fg` for translocations' dose estimation module.

## Fixes

* Fixed dependency of the "Calculate fitting" button in the translocation module.
* Fixed calculations for whole and partial body estimations for translocations module.
* Fixed colors on chromosome hot table example.
* Fix genomic conversion factor not being read when doing fitting.
* Fix `Fp -> y` when using dicentrics in dose estimation module.
* Fixed detection limit calculation.
* Fixed dose estimation in detection limits function.
* Corrected correction of FISH to Full genome when inputting manual fitting curve in dose estimation modules.
* Corrected reading of translocations fit curve values in dose estimation module.

## Improvements

* Tweaked variable names and order in cases data for translocations dose estimation.
* Updated `help_colors_dialog` for translocations modules.
* New module for auxiliary `rhandsontable()` tables used in help dialogues.
* New help dialogues for translocations modules.
* Consistent use of `genome_fraction` in variables related to the genomic conversion factor/fraction of genome hybridised.
* ggplot2 curve generation is now wrapped inside a function on the dose estimation modules.
* Finished confounders help dialogue.
* New aberration calculation method using `purrr::map_df()` instead of a nested loops, which is about 10 times faster.
* Make clear in translocations dose estimation module that the coefficients are from the full genome.
* Added highly protracted exposure.
* Implemented calculation of detection limits from data.
* Added dose calculation to detection limits section.
* Changed `σ` to `\sigma` in help dialogues.
* Added units to result tables.
* Code clean-up on delta method for whole-body dose estimation.
* Updated plot CI text generation on legend to account for new Delta method and for separate whole-body & heterogeneous error method selection.
* CONTRIBUTORS.md is now displayed as a modal.
* Using ORCID for authors when possible.
* Updated institution links in contributors files.

## UI Improvements

* Improved `side-widget-tall` bottom margins.
* Moved translocation frequency `selectInput()` next to Calculate Fitting button for better UX.
* New `hot-improved` CSS class for better hot tables' formatting.
* All hot tables are using `hot-improved` class now.
* New contributors list style and custom CSS style for tables.
* Added experiment/aberration name on tab body title to avoid confusions.
* Improved "About" tab paddings.
* Improve widths of color columns in chromosome tables.
* Narrower color columns in chromosome tables.
* All hot tables' widths are dynamic or exact now.
* Improved small action buttons style on help dialogues.
* New results color to differentiate from main color.
* Updated UI to adapt for new Delta method for whole-body estimation.
* Updated translocations UI to adapt for new Delta method for whole-body estimation.
* Added CI for Dolphin dose estimation UI.
* Added (work in progress) Micronuclei into UI.
* Hide language selector until shiny-i18n is implemented.


# biodosetools 1.2.0 (2019-06-25)

## New features

* Start translocations dose estimation module and report.
* Allow using M-Fish color scheme in translocations fitting module.
* Added method to convert from translocations measured by FISH to full genome.
* Implemented confounders modifications for translocations in `generalEstimateCaseHotTable()` module.

## Improvements

* New chromosome table structure. `transChromosomeTable()` and `transFractionToFullGenomeCalc()` have been moved to `transGeneralModule.R`, as they are reused in the dose estimation module.
* Using new chromosome table UI and server modules for translocations dose estimation.
* Merged "repeated" fitting modules into a `generalFittingModules.R` file.
* Merged `*FittingResults()` modules into `generalFittingResults()` module.
* Updated translocations fitting report to new chromosome table structure.
* New `generalEstimateFittingCurveHotTables()` and `generalEstimateFittingCurve()` modules.
* New `globalVariables.R` with `global_fitting_formulas` list.
* Added translocations-specific code in `generalEstimateFittingCurve()` module to take into account used translocation frequency.
* New inputs for used translocation frequency and genomic conversion factor in manual fitting input in dose estimation module.
* Moved dose estimation to `generalEstimateResults()` module.
* Modify translocations `(X - Xt)` for whole-body and partial-body dose estimation.
* Added readOnly status to `Xt`, `yt`, `yt_err` variables on `rhandsontable()` for translocations case data.

## UI improvements

* New `innerColumn` to use inside cards/boxes.
* New `widgetLabel()` function for labels above widgets without default label.
* Replaced old labels by new `widgetLabel()` function.
* Finished confounders UI in translocations dose estimation module.
* New `mySwichInput()` function based on `shinyWidgets::switchInput()` with `sideLabel` and custom status colors.
* Moved from `awesomeCheckbox()` to `mySwitchInput()` in all UI modules.
* Moved fitting curve input in translocations' dose estimation module to the top.

## Fixes

* Fix minor error in calculation of infimum yield values.
* Fixed "Color options" card and color_list object for chromosome table generation.
* Fixed missing type argument in `get_model_statistics()` in dose estimation module.
* Deleted heterogeneous estimation for translocations.


# biodosetools 1.1.0 (2019-06-15)

## New features

* Conditional tabs in sidebar by implementing `bs4MyConditionalSidebarMenuItem()` function.
* Implemented AIC calculation for different dose estimation assessments.
* Added ZIP archive with sample data for testing.
* New dynamic `bs4TabCard()` using `renderUI()` and `session$ns()` to show different tabs depending on user input.
* Implemented manual calculation of case parameters (`N`, `X`, `y`, `y_est`, `DI`, `u`) in dose estimation module.

## Improvements

* Disabled automatic calculation of `N`, `X`, `DI`, `u` to avoid broken input issues when running the deployed app.
* Added `CONTRIBUTORS.md` file.
* Removed {fontawesome} package requirement.
* Added translocation frequency and genomic conversion factor to translocations fitting report.
* Chromosome table included in reports.
* Major rewrite of {ggplot2} plot in dose estimation module.
* Added Dolphin CI info into plot when using partial-body assessment.
* Changed display of CI in plot for dose estimation.
* Genomic conversion factor is used in `transFittingResults()` module for calculations instead of modifying the value of `N` in `transFittingHotTable()`.
* Added theoretical calculation of model-specific statistics (`glm` method) on fitting module.
* Added renormalisation of data for proper calculation of model-specific statistics in fitting module.
* Added theoretical model-specific statistics calculation for constraint `maxlik` optimization method in fitting module.
* Added global `get_model_statistics()` to translocations fitting module.

## UI improvements

* Cleaner version of `topButton()` for help button and help modals on fitting and dose estimation modules..
* More informative model summary dialogue in fitting module.
* Better output of genomic conversion factor.
* "Partial" is now "Partial-body". Better titles in dose estimation results card.
* New `bs4MyTabCard()` function with topButton and noPadding capabilities.

## Fixes

* Fixed problem with fit_formula_raw when using linear model. Disabled `*-no-int` models until Dolphin can deal with them.
* Fixed rendering problem in Firefox (and potentially other browsers) with count data `rhandsontable()` using 100% of the card's height.
* Fixed dose estimation `rhandsontables()` `div`s on Firefox having `height = 100%` of the card's own height.
* Fixed height error in cases data table in dose estimation module in Firefox.
* Fixed `glm_results object` issue in `get_model_statistics()` function.


# biodosetools 1.0.0 (2019-05-24)

First public beta for laboratories and research centres.

## New features

* Initial support of reports.
* Implemented selection of CI for dose estimation (83%-83% & 95%-95%).
* Added method selection for partial body exposure estimation.
* Fixed formatting of variance covariance matrices when rendered by `rHandsontableOutput()`.
* Linear models compatibility for partial-body (Dolphin) and heterogeneous dose estimation.
* Experimental version of new Maximum Likelihood Estimation method for dose-effect fitting.
* Manual input of fitting coefficients and var/cov matrix for dose estimation.

## Improvements

* Added format/MIME type validation for file inputs.
* Render fitting formula using MathJax.
* Format curve depending on selected assessment/method (use appropriate CI) on dose estimation.
* Check if yield projection into dose is mathematically possible. Negative values of yield or dose are changed to zero as well.
* Added modification to Merkle's yield error function when using protracted exposures.
* Implemented protracted exposure for partial body dose estimation.
* Added automatic Poisson/quasi-Poisson selection on fitting.
* Correct p-values depending on fitting model dispersion.
* Added automatic correction of confidence intervals to use "simplified" Merkle's method if necessary.

## Fixes

* Fixed calculation of full genome.
* Fixed alignment of conditional error/method inputs for dose estimation.
* Changed "base" to "estimate" on CI tables for dose estimation.
* Fixed `bs4TabCard()` bug.


# biodosetools 0.2.0 (2019-04-03)

Version presented in second Team Meeting at Munich.

## New features

* Migrated UI from {shinydashboard} to {bs4Dash}.
* Added data, plot, and results export options.
* Added modal dialogues using {shinyBS}.
* Dose estimation modules for dicentrics.
* Complete implementation of dicentrics analysis.
* Experimental fitting for translocations assay.


# biodosetools 0.1.0 (2018-11-29)

First draft and proof of concept presented to the RENEB team at Barcelona.

## Features

* UI built using {shinydashboard}.
* Fitting modules for dicentrics.
* Dynamic input tables powered by {rhandsontable}.
