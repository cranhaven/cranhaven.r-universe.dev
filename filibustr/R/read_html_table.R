#' Scrape an online HTML table
#'
#' `read_html_table()` returns an HTML table at a specified URL and CSS element
#'  as a dataframe.
#'
#' @param url A string giving the URL to read from.
#' @param css A string giving the CSS element to select.
#'
#'  SelectorGadget
#'  (<https://selectorgadget.com/>) is a useful tool for finding the code for the
#'  CSS element you need. See [rvest::html_element()] for more information.
#'
#' @return A tibble.
#' @export
#'
#' @examplesIf !is.null(curl::nslookup("voteview.com", error = FALSE))
#' # The table used in `get_senate_cloture_votes()`
#' # NOTE: `get_senate_cloture_votes()` performs some cleaning on this table
#' read_html_table(url = "https://www.senate.gov/legislative/cloture/clotureCounts.htm",
#'                 css = ".cloturecount")
#'
#' @examplesIf !is.null(curl::nslookup("www.baseball-reference.com", error = FALSE))
#' # A table from Baseball Reference
#' read_html_table(url = "https://www.baseball-reference.com/friv/rules-changes-stats.shtml",
#'                 css = "#time_of_game")
read_html_table <- function(url, css) {
  rvest::read_html(url) |>
    rvest::html_element(css = css) |>
    rvest::html_table()
}
