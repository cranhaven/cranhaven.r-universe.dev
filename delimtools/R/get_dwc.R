#' Get Darwin Core Terms and Definitions
#'
#' @description
#' `get_dwc()` returns a list of standardized terms and definitions used by the Darwin Core
#' Maintenance Interest Group <https://dwc.tdwg.org/>.
#'
#' @param type Which type of distribution files to download. Available options are:
#' \itemize{
#' \item simple Simple Darwin Core Terms.
#' \item all All Darwin Core Terms.
#' }
#'
#' @details
#' `get_dwc()` reads Darwin Core distribution documents and terms from Github repository
#' <https://github.com/tdwg/dwc> directly into `Environment`. This function will return a list
#' containing the most recent accepted terms as a vector and a [tbl_df][tibble::tbl_df] containing
#' terms, definitions, examples and details about each one of them.
#'
#' @return
#' a list.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins
#' 
#' @examples
#' dwc <- get_dwc(type= "simple") 
#'
#' @export

get_dwc <- function(type){
  
  # check if `readr` is installed
  rlang::check_installed("readr", reason= "to run `get_dwc` properly.")

  dwc <- readr::read_csv(glue::glue("https://raw.githubusercontent.com/tdwg/dwc/master/dist/{type}_dwc_vertical.csv"), show_col_types = FALSE)
  terms <- readr::read_csv("https://raw.githubusercontent.com/tdwg/dwc/master/vocabulary/term_versions.csv", show_col_types = FALSE)
  terms <- terms |>
    dplyr::filter(.data$term_localName %in% dplyr::pull(dwc, type) & .data$status == "recommended") |>
    dplyr::distinct(.data$term_localName, .keep_all = TRUE)

  return(list(dwc= dplyr::pull(dwc, type), terms= terms))
}
