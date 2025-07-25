#' Load cbb play by play data
#'
#' @description Grabs entire seasons of play-by-play data from the collegebasketballdata.com API for specified season(s)
#'
#' Includes a roughly complete record of every tracked play in for the games in the specified seasons.
#'
#' For many games with clean player substitution data, the identities of the players on the floor are included. Sometimes, substitution data is unclean, and this results in too many or too fewer players listed as being on the floor at a given time.
#'
#' A primary player is identified for some plays as `participants_*_1`. For example, in an unassisted shot, the ID of the player taking the shot is `participants_id_1`. If two players are involved in a play, the initiating player is listed as `participants_id_1` and the receiving player is listed as `participants_id_2`. If a player passes the ball to another player who makes a shot, the assisting player is listed as `participants_id_1` and the shooting player is listed as `participants_id_2`.
#'
#' Shot location data is provided where available.
#'
#' The win probability for the home team at the time of each play is provided as `win_probability`. These are predictions provided by the collegebasketballdata.com API, not built into the package.
#'
#' @param seasons an integer or vector of integers of seasons to fetch data for. Defaults to the most recent season. Pass in `TRUE` to fetch all seasons.
#'
#' @return a dataframe of play-by-play data from all D1 college basketball games.
#'
#' @examples
#' \donttest{
#' try({ # avoid cran errors
#' load_plays()
#' })
#' }
#'
#' @seealso <https://john-b-edwards.github.io/cbbreadr/articles/dictionary_plays.html> for a web version of the data dictionary
#' @seealso [`dictionary_plays`] for the data dictionary as bundled within the package
#' @seealso Issues with this data should be filed here: <https://github.com/john-b-edwards/cbbd-data/issues>
#'
#' @export
load_plays <- function(seasons = most_recent_season()) {
  seasons <- check_seasons_legit(seasons, first_season = 2006)
  urls <- glue::glue(
    "{RELEASE_URL}/plays/plays_{seasons}.rds"
  )
  out <- nflreadr::load_from_url(urls, seasons = seasons)
  return(out)
}
