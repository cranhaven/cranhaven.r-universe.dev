# survivalAnalysis 0.4.0

* survival_rates and write_survival_rates: Compute survival rates by KM estimate for given time points    for an univariate survival analysis.
* allow to display multiple KM curves (not comparisons) in the same KM plot
  via in_one_kaplan_meier_plot
* mv analysis: add support for interactions
* kaplan_meier_plot: add parameter to allow to add ggplot objects to plot (to specify line type e.g.)
* uv analysis: Support subgroup_n in cases where a uv analysis with a factor is displayed
* bug fix: adjust days per months constant to 30.4375 (previously 30.5, exact 30.436875) so days per year is an exact multiple
* bug fix in pluck_survival_analysis
* bug fix in write_survival: need to remove wrapping list for output to work correctly with multiple objects
* fix NOTEs on CRAN with R-devel

# survivalAnalysis 0.3.0

* add the write_survival method allowing to write the formatted summary
  of an survival analysis result into a text file, for use when
  writing your paper
* adapt to API changes/lifecycle in dependencies

# survivalAnalysis 0.2.0

* add the methods pluck_survival_analysis and pluck_multivariate_analysis
  providing for the first clean API to access the "black-box" result objects
* add the multivariate_as_data_frame method to access the multivariate result object
* fix bug when cleaning column names in analyse_ functions

# survivalAnalysis 0.1.3

* fix correct extraction of p value from survdiff in case degree-of-freedom is >1 (thanks to Nolan A. Wages)

# survivalAnalysis 0.1.2

* fix crash with upcoming dplyr 1.0.0, and some rlang deprecations
* fix x scaling and breaking in KM plot

# survivalAnalysis 0.1.1

* export identity_order, which is used by forest plots
* work around upstream name collision (flatten_raw in purrr and rlang)

# survivalAnalysis 0.1.0

* initial release
