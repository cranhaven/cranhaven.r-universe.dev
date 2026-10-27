#' Get U SPORTS football scoring summaries
#'
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name       | Type |
#' |:-----------------|:-----|
#' |prd               | chr  |
#' |time              | chr  |
#' |scoring_summary   | chr  |
#' |away_score        | int  |
#' |home_score        | int  |
#' |away_team         | chr  |
#' |home_team         | chr  |
#' |game_id           | chr  |
#' |season            | chr  |
#' |season_type       | chr  |
#'
#' @examples
#' try(football_scoring_summaries(2018))
#' @export

football_scoring_summaries <- function(year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  seasons <- create_season(year)
  sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/fball_scoring_summaries/fball_scoring_summaries_",season, ".csv")) %>%
    lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
}

