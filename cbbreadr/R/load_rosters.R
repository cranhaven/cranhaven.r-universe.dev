#' Load cbb team rosters
#'
#' @description Pull in rosters for college basketball teams for specified season(s)
#'
#' @param seasons an integer or vector of integers of seasons to fetch data for. Defaults to the most recent season. Pass in `TRUE` to fetch all seasons.
#'
#' @return a dataframe of rosters for college basketball teams in the specified season(s).
#'
#' @examples
#' \donttest{
#' try({ # avoid cran errors
#' load_rosters()
#' })
#' }
#'
#' @seealso <https://john-b-edwards.github.io/cbbreadr/articles/dictionary_rosters.html> for a web version of the data dictionary
#' @seealso [`dictionary_rosters`] for the data dictionary as bundled within the package
#' @seealso Issues with this data should be filed here: <https://github.com/john-b-edwards/cbbd-data/issues>
#'
#' @export
load_rosters <- function(seasons = most_recent_season()) {
  seasons <- check_seasons_legit(seasons)
  urls <- glue::glue(
    "{RELEASE_URL}/rosters/rosters_{seasons}.rds"
  )
  out <- nflreadr::load_from_url(urls, seasons = seasons)
  return(out)
}
