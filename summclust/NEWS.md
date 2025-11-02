# New features since latest release

# summclust 0.7.2

- change examples to meet [CRAN tests](https://www.stats.ox.ac.uk/pub/bdr/donttest/summclust.out) - examples now 
  compute on mtcars instead of a data set that needs to be downloaded
- adds gracefull error message in tests (all skipped on CRAN) when test data cannot be 
  loaded

# summclust 0.7

* fixes a bug that turned NA values in cluster variables not contained in the model call into a distinct cluster. Instead, `summclust` now throws an error.

# summclust 0.6

* support for regression weights / weighted least squares
* prettier errors and warnings via `cli` (0.5.2)

# summclust 0.5

* Initial version released on CRAN. 
