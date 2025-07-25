#' Load cbb player stats
#'
#' @description Pull in statistics aggregated by player for specified season(s)
#'
#' This data includes both traditional box score stats (such as points, OREB, AST) as well as more advanced stats (such as USG%, ORTG). For more information on each stat, see the data dictionary `dictionary_player_stats`.
#'
#' @param seasons an integer or vector of integers of seasons to fetch data for. Defaults to the most recent season. Pass in `TRUE` to fetch all seasons.
#'
#' @return a dataframe of player statistics for college basketball players
#'
#' @examples
#' \donttest{
#' try({ # avoid cran errors
#' load_player_stats()
#' })
#' }
#'
#' @seealso <https://john-b-edwards.github.io/cbbreadr/articles/dictionary_player_stats.html> for a web version of the data dictionary
#' @seealso [`dictionary_player_stats`] for the data dictionary as bundled within the package
#' @seealso Issues with this data should be filed here: <https://github.com/john-b-edwards/cbbd-data/issues>
#'
#' @export
load_player_stats <- function(seasons = most_recent_season()) {
  seasons <- check_seasons_legit(seasons)
  urls <- glue::glue(
    "{RELEASE_URL}/player_stats/player_stats_{seasons}.rds"
  )
  out <- nflreadr::load_from_url(urls, seasons = seasons)
  return(out)
}
