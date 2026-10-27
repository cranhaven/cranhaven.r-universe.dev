#' Get U SPORTS football drive summaries
#'
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name   | Type |
#' |:-------------|:-----|
#' |team          | chr  |
#' |qtr           | chr  |
#' |start         | chr  |
#' |poss          | chr  |
#' |began         | chr  |
#' |plays         | int  |
#' |yards         | int  |
#' |result        | chr  |
#' |away_team     | chr  |
#' |home_team     | chr  |
#' |game_id       | chr  |
#' |season        | chr  |
#' |season_type   | chr  |
#'
#' @examples
#' try(football_drive_summaries(2018))
#' @export

football_drive_summaries <- function(year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  seasons <- create_season(year)
  sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/fball_drive_summaries/fball_drive_summaries_",season, ".csv")) %>%
    lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
}

