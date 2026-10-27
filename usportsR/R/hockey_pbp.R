#' Get U SPORTS hockey pbp
#'
#' @param gender Gender of athletes "m" or "w"
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name   | Type |
#' |:-------------|:-----|
#' |event         | chr  |
#' |period        | chr  |
#' |game_id       | chr  |
#' |away_team     | chr  |
#' |home_team     | chr  |
#' |season        | chr  |
#' |season_type   | chr  |
#'
#' @examples
#' \donttest{try(hockey_pbp("m",2018))}
#' @export

hockey_pbp <- function(gender, year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  if (gender == "m"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/mice_pbp/mice_pbp_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else if(gender == "w"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/wice_pbp/wice_pbp_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else{
    stop("Invalid input: 'gender' must be one of 'm' or 'w'")
  }
}

