# natstrat 2.0.0 (2021-10-12)

This version adds several new functionalities:
  * Multiple treatment or control groups
  * Multiple separate comparisons, using various subsets of the treatment and control groups,
  for which units are chosen in order to balance covariate distributions 
  for all comparisons simultaneously

Several changes to the interface have been made:

* `z` should generally be a factor instead of a vector as before
* `treated` and `control` specifications, if needed, should each be a level of `z`
* many arguments can be specified for each of the treatment levels
    * `q_s`, `max_entry_s` can have a row per treatment level
    * `ratio`, `max_ratio` can have an entry per treatment level
* inputs for the supplemental comparison have been added across the functions: `q_star_s`,
`ratio_star`, `treated_star`, `weight_star`
    
There are several changes to the outputs:

* `optimize_controls` now has only one version of `eps`, `objective`, `objective_wo_importances` 
instead of a raw and regular version. The version now reported is the raw version, not corrected
for missingness. If you would like corrected versions, refer to the standardized differences
outputted by `check_balance` instead
* `generate_constraints` now returns only standardized outputs, not centered. The centering
now takes place within `optimize_controls` instead



# natstrat 1.0.0 (2021-05-17)

The first released version.
