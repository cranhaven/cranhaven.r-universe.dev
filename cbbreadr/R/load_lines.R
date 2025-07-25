#' Load betting information for cbb games
#'
#' @description Pull in vegas odds (overs, unders, spreads, moneylines, etc.) for college basketball games.
#'
#' @param seasons an integer or vector of integers of seasons to fetch data for. Defaults to the most recent season. Pass in `TRUE` to fetch all seasons.
#'
#' @return a dataframe of betting information for all games for the specified season(s).
#'
#' @examples
#' \donttest{
#' try({ # avoid cran errors
#' load_lines()
#' })
#' }
#'
#' @seealso <https://john-b-edwards.github.io/cbbreadr/articles/dictionary_lines.html> for a web version of the data dictionary
#' @seealso [`dictionary_lines`] for the data dictionary as bundled within the package
#' @seealso Issues with this data should be filed here: <https://github.com/john-b-edwards/cbbd-data/issues>
#'
#' @export
load_lines <- function(seasons = most_recent_season()) {
  seasons <- check_seasons_legit(seasons, first_season = 2013)
  urls <- glue::glue(
    "{RELEASE_URL}/lines/lines_{seasons}.rds"
  )
  out <- nflreadr::load_from_url(urls, seasons = seasons)
  return(out)
}
