#' Get U SPORTS soccer team box score data
#'
#' @param gender Gender of athletes "m" or "w"
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name      | Type |
#' |:----------------|:-----|
#' |X                | int  |
#' |team             | chr  |
#' |goals_1          | int  |
#' |goals_2          | int  |
#' |goals_total      | int  |
#' |shots_1          | int  |
#' |shots_2          | int  |
#' |shots_total      | int  |
#' |saves_1          | int  |
#' |saves_2          | int  |
#' |saves_total      | int  |
#' |fouls_1          | int  |
#' |fouls_2          | int  |
#' |fouls_total      | int  |
#' |corners_1        | int  |
#' |corners_2        | int  |
#' |corners_total    | int  |
#' |game_date        | lgl  |
#' |game_id          | chr  |
#' |goals_ot         | int  |
#' |goals_2ot        | int  |
#' |shots_ot         | int  |
#' |shots_2ot        | int  |
#' |saves_ot         | int  |
#' |saves_2ot        | int  |
#' |fouls_ot         | int  |
#' |fouls_2ot        | int  |
#' |corners_ot       | int  |
#' |corners_2ot      | int  |
#' |season           | chr  |
#' |season_type      | chr  |
#'
#' @examples
#' try(soccer_team_box_score("m",2018))
#' @export

soccer_team_box_score <- function(gender,year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  if (gender == "m"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/msoc_team_box/msoc_team_box_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else if(gender == "w"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/wsoc_team_box/wsoc_team_box_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else{
    stop("Invalid input: 'gender' must be one of 'm' or 'w'")
  }
}

