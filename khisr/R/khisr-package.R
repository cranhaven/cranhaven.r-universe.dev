#' @keywords internal
#' @import dplyr
#' @import tidyr
#' @import rlang
#' @import httr2
"_PACKAGE"

## usethis namespace: start
#' @importFrom janitor clean_names
#' @importFrom lifecycle deprecate_warn
#' @importFrom lifecycle deprecated
#' @importFrom lifecycle is_present
#' @importFrom lubridate month
#' @importFrom lubridate today
#' @importFrom lubridate year
#' @importFrom lubridate ym
#' @importFrom lubridate ymd
#' @importFrom purrr map
#' @importFrom purrr map_vec
#' @importFrom purrr pluck
#' @importFrom purrr pmap_chr
#' @importFrom stringr str_c
#' @importFrom stringr str_dup
#' @importFrom stringr str_extract
#' @importFrom stringr str_glue
#' @importFrom stringr str_remove
#' @importFrom stringr str_to_upper
## usethis namespace: end
NULL

#' khisr Configuration
#'
#' @description
#' Some aspects of khisr behaviour can be controlled via an option.
#'
#' @section Messages:
#'
#' The `khis_quiet` option can be used to suppress messages form khisr. By
#'   default, khisr always messages, i.e. it is *not* quiet.
#'
#' set `khis_quiet` to `TRUE` to suppress message, by one of these means,
#'   in order of decreasing scope:
#' * Put `options(khis_quiet = TRUE)` in the start-up file, such as `.Rprofile`,
#'   or  in your R script.
#' * Use `local_khis_quiet()` to silence khisr in a specific scope.
#' * Use `with_khis_quite` to run small bit of code silently.
#'
#' `local_khis_quiet` and `with_khis` follow the conventions of the
#'   [withr](https://withr.r-lib.org) package.
#'
#' @return No return value, called for side effects
#'
#' @name khisr-configuration
NULL
