#' Load cbb conference information
#'
#' @description Loads information about college basketball conferences
#'
#' @return a dataframe of information about college basketball conferences.
#'
#' @examples
#' \donttest{
#' try({ # avoid cran errors
#' load_conferences()
#' })
#' }
#'
#' @seealso <https://john-b-edwards.github.io/cbbreadr/articles/dictionary_conferences.html> for a web version of the data dictionary
#' @seealso [`dictionary_conferences`] for the data dictionary as bundled within the package
#' @seealso Issues with this data should be filed here: <https://github.com/john-b-edwards/cbbd-data/issues>
#'
#' @export
load_conferences <- function() {
  out <- nflreadr::load_from_url(
    glue::glue("{RELEASE_URL}/conferences/conferences.rds")
  )
  return(out)
}
