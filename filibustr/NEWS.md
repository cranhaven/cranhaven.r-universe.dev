# filibustr 0.2.1

## Bug fixes

* Handle HTTP errors when using online connections (#12)

# filibustr 0.2.0

## Breaking changes

* `get_lhy_data()` has been renamed to `get_hvw_data()`.
* In `get_voteview_*()`, `chamber` and `congress` now come before `local` and 
  `local_dir`. This matches the argument order in `get_hvw_data()` (#3).

## New features

* New function `get_les()` retrieves Legislative Effectiveness Scores data from the Center 
  for Effective Lawmaking (#5).

# filibustr 0.1.1

* Fixes for CRAN.

# filibustr 0.1.0

* Initial CRAN submission.
