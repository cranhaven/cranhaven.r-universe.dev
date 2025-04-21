#' mnis: Easy Downloading Capabilities for the Member Name Information Service
#'
#' An API package for the Members' Name Information Service operated by the
#' UK parliament. The package is intended to simplify pulling data from an API
#' for users unfamiliar with APIs. Documentation for the API itself can be
#' found here: http://data.parliament.uk/membersdataplatform/default.aspx.
#'
#' The package includes a built in function to remove a byte-order mark from
#' the API data, and a parameter `tidy` with each function that converts
#' variable names into an R friendly style, removing non-alphanumeric
#' characters and converting to snake_case when equal to `TRUE`,
#' its default value.
#'
#' All functions requests data in JSON format and parse it to a tibble. The
#' exception is [mnis_constituency_results()] which returns a
#' single object containing a list (with constituency details) and a
#' tibble (with election results).
#'
#' None of the functions included in this package appear to run into the
#' API rate limit, although there may be restrictions to custom requests,
#' which allow a maximum of three parameters.
#'
#'
#' @docType package
#' @name mnis
#' @importFrom httr GET http_type accept_json
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr inner_join
#' @importFrom tibble as_tibble tibble
#' @importFrom stringi stri_trans_general
#' @importFrom janitor make_clean_names
#' @importFrom purrr discard
#' @useDynLib mnis, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL
