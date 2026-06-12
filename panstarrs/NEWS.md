# panstarrs 0.2.3

## Bug fixes and minor changes

-  Small changes

# panstarrs 0.2.2

## Bug fixes and minor changes

-   Small changes

# panstarrs 0.2.1

## Bug fixes and minor changes

-   The example with creating guiding pictures was removed due to the unavailability of [magicaxis](https://cran.r-project.org/package=magicaxis) package on CRAN.

# panstarrs 0.2.0

## New features

-   Functions `ps1_search(), ps1_cone(), ps1_crossmatch()` now support *Forced Mean* for DR2 release.
-   If there is no internet connection or the server API does not work, then `invisible(NULL)` is returned and an error message appears.

## Bug fixes and minor changes

-   Functions related to catalog data retrieval return data types according to metadata. In particular, this applies to columns with the `int64` type (thanks to bit64 package).

-   Some dependencies have been removed: attempt, dplyr, glue, magrittr, purrr, Rcurl, readr, rlang, stringr.

-   HTTP User Agent "panstarrs/{version} (<https://CRAN.R-project.org/package=panstarrs>)" has been added to GET/POST requests.

-   Vignettes have been updated.

-   Some internal functions has been removed.

# panstarrs 0.1.0

-   For CRAN release.
