#' @description
#' cancerscreening provide a easy way to download cancer screening data from the
#'   Kenya Health Information System (KHIS) using R.
#'
#' Most function begin with the prefix `get_` followed by the screening area
#' `cervical` or `breast`. The Goal is to allow the download of data associated
#' with the data of interest, e.g. `get_cervical_screened`, `get_cervical_positive`,
#' or `get_cervical_treated`.
#'
#' cancerscreening is "pipe-friendly" and, in fact re-exports `%>%` but does not
#' require its use.
#'
#' Please see the cancerscreening website for full documentation:
#'   * <https://cancerscreening.damurka.com/index.html>
#'
#' In addition to function-specific help, there are several articles which are
#' indexed here:
#' * [Article index](https://cancerscreening.damurka.com/articles/index.html)
#'
#' @keywords internal
#' @import dplyr
#' @import khisr
#' @import tidyr
#' @import rlang
"_PACKAGE"

## usethis namespace: start
#' @importFrom lubridate quarter
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_glue
#' @importFrom stringr str_remove
## usethis namespace: end
NULL

#' cancerscreening configuration
#'
#' @description
#' Some aspects of cancerscreening behaviour can be controlled via an option.
#'
#' @section Messages:
#'
#' The `cancerscreening_quiet` option can be used to suppress messages form
#' cancerscreening. By default, cancerscreening always messages, i.e. it is *not*
#' quiet.
#'
#' set `cancerscreening_quiet` to `TRUE` to suppress message, by one of these
#' means, in order of decreasing scope:
#' * Put `options(cancerscreening_quiet = TRUE)` in the start-up file, such as
#'   `.Rprofile`, or  in your R script
#' * Use `local_cancerscreening_quiet()` to silence cancerscreening in a specific
#'   scope
#' * Use `with_cancerscreening_quite` to run small bit of code silently
#'
#' `local_cancerscreening_quiet` and `with_cancerscreening` follow the conventions
#' of the withr package (<https://withr.r-lib.org>).
#'
#' @return No return value, called for side effects
#'
#' @name cancerscreening-configuration
NULL
