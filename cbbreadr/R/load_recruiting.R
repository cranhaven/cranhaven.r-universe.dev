#' Load recruiting data
#'
#' @description Loads information about college basketball recruiting
#'
#' @return a dataframe of information about college basketball recruits
#'
#' @examples
#' \donttest{
#' try({ # avoid cran errors
#' load_recruiting()
#' })
#' }
#'
#' @seealso <https://john-b-edwards.github.io/cbbreadr/articles/dictionary_recruiting.html> for a web version of the data dictionary
#' @seealso [`dictionary_recruiting`] for the data dictionary as bundled within the package
#' @seealso Issues with this data should be filed here: <https://github.com/john-b-edwards/cbbd-data/issues>
#'
#' @export
load_recruiting <- function() {
  out <- nflreadr::load_from_url(
    glue::glue("{RELEASE_URL}/recruiting/recruiting.rds")
  )
  return(out)
}
