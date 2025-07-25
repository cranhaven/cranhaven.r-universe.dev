#' Load cbb teams stats
#'
#' @description Pull in statistics aggregated by team for for specified season(s)
#'
#' @param seasons an integer or vector of integers of seasons to fetch data for. Defaults to the most recent season. Pass in `TRUE` to fetch all seasons.
#'
#' @return a dataframe of team statistics for college basketball teams.
#'
#' @examples
#' \donttest{
#' try({ # avoid cran errors
#' load_team_stats()
#' })
#' }
#'
#' @seealso <https://john-b-edwards.github.io/cbbreadr/articles/dictionary_team_stats.html> for a web version of the data dictionary
#' @seealso [`dictionary_team_stats`] for the data dictionary as bundled within the package
#' @seealso Issues with this data should be filed here: <https://github.com/john-b-edwards/cbbd-data/issues>
#'
#' @export
load_team_stats <- function(seasons = most_recent_season()) {
  seasons <- check_seasons_legit(seasons)
  urls <- glue::glue(
    "{RELEASE_URL}/team_stats/team_stats_{seasons}.rds"
  )
  out <- nflreadr::load_from_url(urls, seasons = seasons)
  return(out)
}
