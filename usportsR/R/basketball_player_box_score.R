
#' Get U SPORTS basketball player box scores
#'
#' @param gender Gender of athletes "m" or "w"
#' @param year The season(s) of interest.
#' @returns
#' |Column Name      | Type |
#' |:----------------|:-----|
#' |player           | chr  |
#' |min              | int  |
#' |fgm              | int  |
#' |fga              | int  |
#' |three_pm         | int  |
#' |three_pa         | int  |
#' |ftm              | int  |
#' |fta              | int  |
#' |oreb             | int  |
#' |dreb             | int  |
#' |reb              | int  |
#' |ast              | int  |
#' |stl              | int  |
#' |blk              | int  |
#' |to               | int  |
#' |pf               | int  |
#' |pts              | int  |
#' |starter          | int  |
#' |player_number    | int  |
#' |player_links     | chr  |
#' |date             | chr  |
#' |game_id          | chr  |
#' |team_name        | chr  |
#' |season           | chr  |
#' |season_type      | chr  |
#'
#' @examples
#' try(basketball_player_box_score("m",2018))
#' @export


basketball_player_box_score <- function(gender,year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  if (gender == "m"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/mbkb_player_box/mbkb_player_box_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else if(gender == "w"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/wbkb_player_box/wbkb_player_box_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else{
    stop("Invalid input: 'gender' must be one of 'm' or 'w'")
  }
}

