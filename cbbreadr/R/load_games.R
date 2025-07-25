#' Load college basketball games for specified season(s)
#'
#' @description Pull in all college basketball games tracked by collegebasketballdata.com
#'
#' Returns data for all NCAA D1 men's basketball games for the specified seasons. Includes contextual information, such as where the game took place and who it is between, as well as high level box score data, such as the scores of each team.
#'
#' This will return both games in the past and scheduled games. If a game is scheduled but its opponents have not yet been determined (i.e., the national championship game of the upcoming season), it will not appear in this dataset.
#'
#' @param seasons an integer or vector of integers of seasons to fetch data for. Defaults to the most recent season. Pass in `TRUE` to fetch all seasons.
#'
#' @return a dataframe of all games in the specified season(s).
#'
#' @examples
#' \donttest{
#' try({ # avoid cran errors
#' load_games()
#' })
#' }
#'
#' @seealso <https://john-b-edwards.github.io/cbbreadr/articles/dictionary_games.html> for a web version of the data dictionary
#' @seealso [`dictionary_games`] for the data dictionary as bundled within the package
#' @seealso Issues with this data should be filed here: <https://github.com/john-b-edwards/cbbd-data/issues>
#'
#' @export
load_games <- function(seasons = most_recent_season()) {
  seasons <- check_seasons_legit(seasons)
  urls <- glue::glue(
    "{RELEASE_URL}/games/games_{seasons}.rds"
  )
  out <- nflreadr::load_from_url(urls, seasons = seasons)
  return(out)
}
