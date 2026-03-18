#' Rename Columns using Darwin Core Standard Terms
#'
#' @description
#' `as_dwc()` rename columns in a [tbl_df][tibble::tbl_df] using a vector of terms defined by
#' Darwin Core Standard.
#'
#' @param dwc a list of standard terms and definitions created using [get_dwc()].
#' @param data a [tbl_df][tibble::tbl_df].
#' @param terms a vector or list of terms to be used as replacement.
#'
#' @details
#' `as_dwc()` will replace current column names by the ones defined in `terms`. For each
#' column in `data`, Darwin Core equivalent terms must be informed in the same order
#' by the user. If `terms` and column names do not match in length or if `terms` used
#' are not found in Darwin Core standard, an error will be printed on `Console`.
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df].
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.
#'
#' @examples
#'
#' # get dwc terms and definitions
#' dwc <- get_dwc(type = "simple")
#'
#' # create a data frame with sample metadata
#' my_df <- tibble::tibble(
#'   species = c("sp1", "sp2", "sp3"),
#'   location = c("loc1", "loc2", "loc3"),
#'   voucher = c("M01", "M02", "M03"),
#'   collector = c("John", "Robert", "David")
#' )
#'
#' # rename columns
#' as_dwc(dwc, my_df, terms = c("scientificName", "locality", "catalogNumber", "recordedBy"))
#'
#' @export
as_dwc <- function(dwc, data, terms) {
  data_cols <- colnames(data)

  if (length(data_cols) != length(terms)) {
    cli::cli_abort(c("Your column names and terms are not of equal length:",
      "x" = "You've supplied inputs with different size lengths.",
      "i" = "Your column length: {length(data_cols)}",
      "i" = "Your terms length: {length(terms)}"
    ))
  }

  if (all(terms %in% dwc$dwc)) {
    new_terms <- names(data) |> `names<-`(terms)

    data <- dplyr::rename(data, tidyselect::all_of(new_terms))

    return(data)
  } else {
    missing <- setdiff(terms, dwc$dwc)

    cli::cli_abort(c("Some of your terms do not match Darwin Core list of terms.",
      "x" = "Some terms provided are either non-valid or deprecated.",
      "i" = "Found { length(missing) } unmatched term{?s}:",
      stringr::str_flatten_comma(missing)
    ))
  }
}
