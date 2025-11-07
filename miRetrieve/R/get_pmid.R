#' Get PubMed-IDs of a data frame
#'
#' Get PubMed-IDs of a data frame.
#'
#' Get PubMed-IDs of a data frame. \code{get_pmid} returns either a character
#' vector, containing PubMed-IDs, or copies PubMed-IDs to clipboard. If PubMed-IDs
#' are copied to the clipboard, they can be used e.g. to search for abstracts on
#' PubMed.
#'
#' @param df Data frame containing PubMed-IDs.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param copy Boolean. If `copy = FALSE`, `get_pmid()` returns a character
#' vector, containing PubMed-IDs. If `copy = TRUE`, `get_pmid()` copies
#' PubMed-IDs to clipboard.
#'
#' @return Copy to clipboard or character vector.
#' If `copy = TRUE`, `get_pmid()` copies
#' PubMed-IDs to clipboard.
#' If `copy = FALSE`, `get_pmid()` returns a character
#' vector, containing PubMed-IDs.
#'
#' @family get functions
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom utils write.table
get_pmid <- function(df,
                     col.pmid = PMID,
                     copy = TRUE) {
  pmids <- df %>%
    dplyr::select({{col.pmid}}) %>%
    dplyr::pull() %>%
    unique()

  if (copy == TRUE) {
    clip <- pipe("pbcopy", "w")
    write.table(pmids, file=clip, row.names = FALSE, col.names = FALSE)
    close(clip)
  } else {
    return(pmids)
  }
}
