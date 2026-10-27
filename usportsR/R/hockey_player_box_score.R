#' Get U SPORTS hockey player box scores
#'
#' @param gender Gender of athletes "m" or "w"
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name       | Type |
#' |:-----------------|:-----|
#' |skaters           | chr  |
#' |player_number     | chr  |
#' |pos               | chr  |
#' |g                 | int  |
#' |a                 | int  |
#' |plus_minus        | int  |
#' |s                 | int  |
#' |face_off_won      | int  |
#' |face_off_total    | int  |
#' |pim               | int  |
#' |team              | chr  |
#' |ga                | int  |
#' |goalie_mins       | chr  |
#' |sv                | int  |
#' |shots_against     | int  |
#' |goalie_pim        | int  |
#' |game_id           | chr  |
#' |player_links      | chr  |
#' |season            | chr  |
#' |season_type       | chr  |
#'
#' @examples
#' try(hockey_player_box_score("m",2018))
#' @export

hockey_player_box_score <- function(gender,year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  if (gender == "m"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/mice_player_box/mice_player_box_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else if(gender == "w"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/wice_player_box/wice_player_box_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else{
    stop("Invalid input: 'gender' must be one of 'm' or 'w'")
  }
}



