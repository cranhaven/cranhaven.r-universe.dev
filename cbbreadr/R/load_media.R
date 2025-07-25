#' Load media for cbb games
#'
#' @description Pull in broadcast information for all games in a given season
#'
#' @param seasons an integer or vector of integers of seasons to fetch data for. Defaults to the most recent season. Pass in `TRUE` to fetch all seasons.
#'
#' @return a dataframe of broadcast information for all games for the specified season(s).
#'
#' @examples
#' \donttest{
#' try({ # avoid cran errors
#' load_media()
#' })
#' }
#'
#' @seealso <https://john-b-edwards.github.io/cbbreadr/articles/dictionary_media.html> for a web version of the data dictionary
#' @seealso [`dictionary_media`] for the data dictionary as bundled within the package
#' @seealso Issues with this data should be filed here: <https://github.com/john-b-edwards/cbbd-data/issues>
#'
#' @export
load_media <- function(seasons = most_recent_season()) {
  seasons <- check_seasons_legit(seasons)
  urls <- glue::glue(
    "{RELEASE_URL}/media/media_{seasons}.rds"
  )
  out <- nflreadr::load_from_url(urls, seasons = seasons)
  return(out)
}
