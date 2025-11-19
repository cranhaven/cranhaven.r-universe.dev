#' tern.rbmi Package
#'
#' `tern.rbmi` is an analysis package to create tables, listings and graphs to analyze clinical trials data.
#'
#' @keywords internal
"_PACKAGE"
#' @import rbmi
#' @importFrom rtables make_afun analyze
#' @importFrom formatters with_label
#' @importFrom lifecycle badge
#' @importFrom broom tidy
#' @importFrom magrittr %>%
#' @importFrom tern f_conf_level
#' @import rbmi
NULL

#' Example dataset for `tern.rbmi` package. This is an pool object from the rbmi analysis, see
#' `browseVignettes(package = "tern.rbmi")`
#'
#' `r lifecycle::badge("experimental")`
#'
"rbmi_test_data"
