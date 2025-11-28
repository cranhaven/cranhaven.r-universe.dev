### Literally taken from
### https://rdrr.io/cran/rgl/src/R/pkgdown.R
pkgdown_rdname <- function() {
  getOption("downlit.rdname", "");
}

in_pkgdown <- function() {
  return(
    requireNamespace("pkgdown", quietly = TRUE) && pkgdown::in_pkgdown()
  );
}

in_pkgdown_example <- function() {
  nchar(pkgdown_rdname()) &&
  requireNamespace("downlit", quietly = TRUE) &&
  exists("is_low_change.default", asNamespace("downlit")) &&
  requireNamespace("pkgdown", quietly = TRUE) &&
  exists("pkgdown_print", asNamespace("pkgdown"))
}
