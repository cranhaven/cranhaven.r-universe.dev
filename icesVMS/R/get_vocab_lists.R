#' Lists of Vocabularies
#'
#' Get a list of vocabularies such as country codes or ecoregions.
#'
#' @return a character vector
#'
#' @examples
#' \donttest{
#' ecoregions <- get_ecoregion_list()
#' "Celtic Seas" %in% ecoregions
#' 
#' stat_recs <- get_stat_rec_list()
#' "40F1" %in% stat_recs
#'
#' countries <- get_country_list()
#' "DK" %in% countries
#' }
#' @importFrom icesVocab getCodeList
#' 
#' @rdname getVocabLists
#' @export
get_ecoregion_list <- function() {
  getCodeList(code_type = "Ecoregion")$Description
}

#' @rdname getVocabLists
#'
#' @export
get_stat_rec_list <- function() {
  getCodeList(code_type = "StatRec")$Key
}

#' @rdname getVocabLists
#'
#' @export
get_country_list <- function() {
  getCodeList(code_type = "ISO_3166")$Key
}
