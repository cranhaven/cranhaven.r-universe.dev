#' Load cbb teams
#'
#' @description Pull in information on all teams that played in a given season.
#'
#' @param seasons an integer or vector of integers of seasons to fetch data for. Defaults to the most recent season. Pass in `TRUE` to fetch all seasons.
#'
#' @return a dataframe of all college basketball teams tracked by collegebasketballdata.com for a given season(s).
#'
#' @examples
#' \donttest{
#' try({ # avoid cran errors
#' load_teams()
#' })
#' }
#'
#' @seealso <https://john-b-edwards.github.io/cbbreadr/articles/dictionary_teams.html> for a web version of the data dictionary
#' @seealso [`dictionary_teams`] for the data dictionary as bundled within the package
#' @seealso Issues with this data should be filed here: <https://github.com/john-b-edwards/cbbd-data/issues>
#'
#' @export
load_teams <- function(seasons = most_recent_season()) {
  seasons <- check_seasons_legit(seasons, first_season = 2003)
  urls <- glue::glue("{RELEASE_URL}/teams/teams_{seasons}.rds")
  out <- nflreadr::load_from_url(urls, seasons = seasons)
  return(out)
}
