#' Load cbb team box score stats
#'
#' @description Pull in statistics aggregated by team and game for specified season(s)
#'
#' This data includes both traditional box score stats (such as points, OREB, AST) as well as more advanced stats (such as ORTG, OREB%). For more information on each stat, see the data dictionary `dictionary_player_box_scores`.
#'
#' @param seasons an integer or vector of integers of seasons to fetch data for. Defaults to the most recent season. Pass in `TRUE` to fetch all seasons.
#'
#' @return a dataframe of team box scores.
#'
#' @examples
#' \donttest{
#' try({ # avoid cran errors
#' load_team_box_scores()
#' })
#' }
#'
#' @seealso <https://john-b-edwards.github.io/cbbreadr/articles/dictionary_team_box_scores.html> for a web version of the data dictionary
#' @seealso [`dictionary_team_box_scores`] for the data dictionary as bundled within the package
#' @seealso Issues with this data should be filed here: <https://github.com/john-b-edwards/cbbd-data/issues>
#'
#' @export
load_team_box_scores <- function(seasons = most_recent_season()) {
  seasons <- check_seasons_legit(seasons)
  urls <- glue::glue(
    "{RELEASE_URL}/team_box_scores/team_box_scores_{seasons}.rds"
  )
  out <- nflreadr::load_from_url(urls, seasons = seasons)
  return(out)
}
