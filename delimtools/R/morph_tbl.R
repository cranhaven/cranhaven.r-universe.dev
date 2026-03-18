#' Generating a Morphological Delimitation Table
#'
#' @description
#' `morph_tbl()` returns species partition hypothesis estimated from a prior taxonomic identifications supplied by the user.
#'
#' @param labels Vector of unique sequence ID labels.
#' @param sppVector Vector of corresponding morphological species delimitation groups.
#' @param delimname Character. String to rename the delimitation method in the table. Default to 'morph'.
#'
#' @details
#' `morph_tbl()` uses information in a species name vector to label each unique sample with a number corresponding to this name.
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df].
#'
#' @author
#' Rupert A. Collins
#'
#' @examples
#'
#' # create a tibble
#' morph_df <- morph_tbl(
#'   labels = geophagus_info$gbAccession,
#'   sppVector = geophagus_info$scientificName
#' )
#'
#' # check
#' morph_df
#'
#' @export
morph_tbl <- function(labels, sppVector, delimname = "morph") {
  dname <- rlang::sym(delimname)

  if (missing(labels) | missing(sppVector)) {
    cli::cli_abort("Please provide a vector of sample IDs and/or species identifiers.")
  }

  if (length(labels) != length(sppVector)) {
    cli::cli_abort("Error: Please ensure that your number of labels is the same as your number of delimitations.")
  }

  delim <- tibble::tibble(labels = as.character(labels), sppVector = as.character(sppVector)) |>
    dplyr::group_by(sppVector) |>
    dplyr::mutate(!!dname := dplyr::cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::select(-sppVector)

  return(delim)
}
