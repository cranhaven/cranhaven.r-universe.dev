# tidytidbits 0.3.3

* comply with new CRAN test checking package anchors
  in Rd links to functions in external, non-base packages

# tidytidbits 0.3.2

* comply with new CRAN test checking for coercion 
  of vectors with length > 1 in logical expression

# tidytidbits 0.3.1

* fix deprecated _along methods in rlang which cause CRAN check errors

# tidytidbits 0.3.0

* add categorical_test_by and contingency_table_by for convenient categorical 
  testing in a pipeline. Supersedes cross_tabulate with clean parameters.
* explicitly use a character column for variable value in count_at to be
  consistent with newer versions of replace_na
* use and require dplyr 1.0.0
* hard lifecycle cut for unused and not future proof code: execute_* removed
* hard lifecycle cut for unused method superseded with new dplyr row/column
  wise approaches: add_summary_* (also depending on not future proof code)

# tidytidbits 0.2.3

* remove the interlude() method, which was unused but made assumptions that were not portable to new versions of rlang

# tidytidbits 0.2.2

* Renames to adapt to rlang/tidyverse deprecations and dplyr 1.0.0 release

# tidytidbits 0.2.1

* Remove warning introduced by rlang update:
  when using env_has, extract environment from quosure instead passing the quosure itself

# tidytidbits 0.2.0

* added `count_at` to perform `count_by` for multiple variables and return the aggregated results in one tibble
* added `first_non_nas_at` and `first_which_non_na_at` to retrieve the first non na value, or which values are not na, using column selection like in `dplyr`'s `select`
* fix incompatibility with current release of purrr (now also exporting flatten_raw, as does rlang; extend exception list in NAMESPACE)

# tidytidbits 0.1.0

* initial release
