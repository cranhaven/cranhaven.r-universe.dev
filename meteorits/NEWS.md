# meteorits 0.1.1

## Minor Improvements

* Added CRAN version badge in `README.Rmd`.

* Added a `cran-comments.md` file.

* Added a `NEWS.md` file to track changes to the package.

## Bug fixes

* Fix a bug in `StatStMoE$EStep`: when trying to integrate (case `calcE3 = TRUE`),
  a vector of characters was returned in some case instead of numerics.
