#' Load ranking data
#'
#' @description Loads information about college basketball rankings, polls, etc.
#'
#' At present, only the AP Top 25 and Coaches Polls are available from this function. Ranking, first place votes, and over points are available for each week of the season dating back to 1949.
#'
#' @return a dataframe of information about college basketball rankings and polls
#'
#' @examples
#' \donttest{
#' try({ # avoid cran errors
#' load_rankings()
#' })
#' }
#'
#' @seealso <https://john-b-edwards.github.io/cbbreadr/articles/dictionary_rankings.html> for a web version of the data dictionary
#' @seealso [`dictionary_rankings`] for the data dictionary as bundled within the package
#' @seealso Issues with this data should be filed here: <https://github.com/john-b-edwards/cbbd-data/issues>
#'
#' @export
load_rankings <- function() {
  out <- nflreadr::load_from_url(
    glue::glue("{RELEASE_URL}/rankings/rankings.rds")
  )
  return(out)
}
