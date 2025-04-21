
#' Full Biography
#'
#' Requests all available biographical information for a given member, and
#' returns it in a list
#' @param ID The ID number of the member, using the default MNIS scheme.
#' If `ref_dods` is TRUE, accepts the Dods monitoring scheme instead.
#' If left empty, returns the same data as [mnis_all_members()]
#' with default parameters.
#' @param ref_dods Request based on the Dods monitoring member ID scheme.
#' Defaults to FALSE. If FALSE, requests using the default MNIS identification
#'  scheme.
#' @param tidy Fix the variable names in the tibble to remove non-alphanumeric
#'  characters and superfluous text, and convert variable names to a consistent
#'   style. Defaults to `TRUE`. Currently does not work.
#' @param tidy_style The style to convert variable names to, if `tidy==TRUE`.
#' @seealso [mnis_basic_details()]
#' @export
#' @examples
#' \dontrun{
#' df172 <- mnis_full_biog(172)
#'
#' df500 <- mnis_full_biog(500)
#' }
#'
mnis_full_biog <- function(ID = NULL, ref_dods = FALSE,
                           tidy = TRUE, tidy_style = "snake_case") {
  if (missing(ID)) {
    mem <- mnis_all_members()
  } else {
    ID <- as.character(ID)

    if (ref_dods == TRUE) {
      id_type <- "refDods="
    } else {
      id_type <- "id="
    }

    query <- paste0(base_url, "members/query/", id_type, ID, "/FullBiog")

    mem <- mnis_additional_utility(query)
  }
  mem
}
