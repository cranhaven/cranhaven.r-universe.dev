# fergm 1.1.4
* Updated `compare_predictions()` to deal with updates in the `ergm` S3 simulation functions.
* Removed any import of `ergm` exports given importation of entire `ergm` package.
* Troubleshooting of prior error for simulating from an ERGM by ensuring a network object on the LHS for simulate.ergm is present in global environment.
* Removed any importing of `coef.ergm()` or `simulate.formula()` as they will be deprecated in future `ergm` packages.

# fergm 1.1.3
* Updated `compare_predictions()` to deal with updates in the `ergm` simulation functions.

# fergm 1.1.2
* Updated vignette to ensure consistency in function calls between help pages and vignettes.  

* Updated seed setting argument and functionality of `compare_predictions()` to assist in replicability.  

* Updated NAMESPACE and dependencies to avoid upstream deprecation warnings from `statnet`.

# fergm 1.1.1
* For `compare_predictions()` the denominator used to calculate the percentage of correctly predicted ties was changed from an arbitrary placeholder to the number of dyads within a network.  Prior results based upon this function will be adequate if results were reported as relative (FERGM v. ERGM) comparisons of the percentage of correctly predicted ties.

* For functions taking the `custom_var_names` argument, such as `clean_summary()`, `coef_plot()`, `coef_posterior_density()`, and `fergm_beta_traceplot()`, the default value in prior versions would produce errors if terms other than those specified in the `fergm.fit` formula were included.  For example, if the `nodemix` term was specified in the `form` argument for `fergm()`, then a statistic for each pairing of values may be added to the model estimated.  These functions taking `custom_var_names` have been updated to handle such terms.  

# fergm 0.2.1
* Revised documentation to add clarity.

* Fixed `compare_predictions_test()` function to correctly return `ks.test` output.

# fergm 0.2.0
* Initial package release.
