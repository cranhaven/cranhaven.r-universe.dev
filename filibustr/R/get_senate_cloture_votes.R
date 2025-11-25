#' Senate cloture motions and votes
#'
#' `get_senate_cloture_votes()` returns a tibble of the number of cloture motions,
#' cloture votes, and successful cloture votes in the Senate during each Congress
#' since 1917.
#'
#' The data is sourced from the official Senate website, specifically
#' <https://www.senate.gov/legislative/cloture/clotureCounts.htm>.
#'
#' @returns A tibble with the number of cloture motions, cloture votes, and
#'  successful cloture votes in each Congress.
#' @export
#'
#' @examplesIf !is.null(curl::nslookup("www.senate.gov", error = FALSE))
#' get_senate_cloture_votes()
get_senate_cloture_votes <- function() {
  read_html_table(url = paste0("https://www.senate.gov",
                               "/legislative/cloture/clotureCounts.htm"),
                  css = ".cloturecount") |>
    dplyr::rename_with(function(.name) tolower(stringr::str_replace_all(.name, " ", "_"))) |>
    # remove "Total" row
    dplyr::slice_head(n = -1) |>
    # fix data types
    dplyr::mutate(dplyr::across(!dplyr::any_of("years"), as.integer))
}
