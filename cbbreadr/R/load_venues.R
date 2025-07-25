#' Load cbb venue information
#'
#' @description Loads information about college basketball venues.
#'
#' @return a dataframe of information about college basketball venues.
#'
#' @examples
#' \donttest{
#' try({ # avoid cran errors
#' load_venues()
#' })
#' }
#'
#' @seealso <https://john-b-edwards.github.io/cbbreadr/articles/dictionary_venues.html> for a web version of the data dictionary
#' @seealso [`dictionary_venues`] for the data dictionary as bundled within the package
#' @seealso Issues with this data should be filed here: <https://github.com/john-b-edwards/cbbd-data/issues>
#'
#' @export
load_venues <- function() {
  out <- nflreadr::load_from_url(
    glue::glue("{RELEASE_URL}/venues/venues.rds")
  )
  return(out)
}
