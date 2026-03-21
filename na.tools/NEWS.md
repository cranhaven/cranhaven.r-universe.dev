## Version 0.3.0 

 * add `na.true` and `na.false`
 * Replace `na.most_freq` with `na.mode`

## Version 0.2.1

 * README.md: Fix canonical link to multivariate task view
 * Remove tools directory from task view

## Version 0.2.0

 * Add `NA_explicit_` as an exported constant for explicit categorical values.
 * Convert man to use markdown.
 * Remove old aliases
 

## Version (2018-01-22) 
 
 * Fix `na_replace` (and `na_explicit`) to add levels for values if
   they do not already exist.
 * Add tests
 * Fix documentation

## Version  (2017-08-22)

 * Add na_explicit and na_implicit

## Version 

 * na_replace: revert from using `ifelse` because of edge cases 
 * add `zzz.R`
 * add `NEWS.md`
 * add tests for `na_replace`

## Version 

 * `na_replace` now uses `ifelse` and prevent recycling `value`
