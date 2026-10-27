#' Get U SPORTS football pbp
#'
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name   | Type |
#' |:-------------|:-----|
#' |downs         | chr  |
#' |event         | chr  |
#' |quarter       | chr  |
#' |game_id       | chr  |
#' |away_team     | chr  |
#' |home_team     | chr  |
#' |season        | chr  |
#' |season_type   | chr  |
#'
#' @examples
#' \donttest{try(football_pbp(2018))}
#' @export

football_pbp <- function(year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  seasons <- create_season(year)
  sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/fball_pbp/fball_pbp_",season, ".csv")) %>%
    lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
}

