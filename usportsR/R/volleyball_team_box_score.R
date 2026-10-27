#' Get U SPORTS volleyball team box scores
#'
#' @param gender Gender of athletes "m" or "w"
#' @param year The season(s) of interest
#' @returns
#' |Column Name   | Type |
#' |:-------------|:-----|
#' |set           | int  |
#' |k             | int  |
#' |e             | int  |
#' |ta            | int  |
#' |pct           | dbl  |
#' |team_name     | chr  |
#' |game_id       | chr  |
#' |date_time     | chr  |
#' |points        | int  |
#' |season        | chr  |
#' |season_type   | chr  |
#'
#' @examples
#' try(volleyball_team_box_score("m",2018))
#' @export

volleyball_team_box_score <- function(gender, year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  if (gender == "m"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/mvball_team_box/mvball_team_box_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else if(gender == "w"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/wvball_team_box/wvball_team_box_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else{
    stop("Invalid input: 'gender' must be one of 'm' or 'w'")
  }
}

