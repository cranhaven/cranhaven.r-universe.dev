# harmonydata 0.3.2

## Bug Fixes
* Fixed error when matching an instrument against itself caused by 
  duplicate row names in the matches data frame (#22)

## Improvements
* Match matrix is now returned as a data frame instead of a list
* Response options similarity matrix is now returned as a data frame
* Added check for empty response options matrix to ensure stability
* Added warning for image compression when generating heatmaps
* Added supermarket data clustering example

## New Features
* Users can now select the LLM model used for matching via the `model` 
  parameter in `match_instruments()`
* Added support for additional clustering algorithms: `kmeans`, 
  `deterministic`, `hdbscan` in addition to the default 
  `affinity_propagation`

# harmonydata 0.3.1

* Previous CRAN release
