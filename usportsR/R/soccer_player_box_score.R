#' Get U SPORTS soccer player box score data
#'
#' @param gender Gender of athletes "m" or "w"
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name      | Type |
#' |:----------------|:-----|
#' |X                | int  |
#' |player           | chr  |
#' |sh               | int  |
#' |sog              | int  |
#' |g                | int  |
#' |a                | int  |
#' |min              | int  |
#' |yellow_card      | int  |
#' |red_card         | int  |
#' |player_links     | chr  |
#' |game_id          | chr  |
#' |sog_against      | int  |
#' |ga               | int  |
#' |sv               | int  |
#' |game_date        | chr  |
#' |season           | chr  |
#' |season_type      | chr  |
#'
#' @examples
#' try(soccer_player_box_score("m",2018))
#' @export

soccer_player_box_score <- function(gender,year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  if (gender == "m"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/msoc_player_box/msoc_player_box_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else if(gender == "w"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/wsoc_player_box/wsoc_player_box_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else{
    stop("Invalid input: 'gender' must be one of 'm' or 'w'")
  }
}

