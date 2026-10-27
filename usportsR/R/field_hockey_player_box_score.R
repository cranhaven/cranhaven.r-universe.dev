#' Get U SPORTS field hockey player box scores
#'
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name      | Type |
#' |:----------------|:-----|
#' |player           | chr  |
#' |team             | chr  |
#' |sh               | int  |
#' |sog              | int  |
#' |g                | int  |
#' |a                | int  |
#' |ds               | int  |
#' |min              | int  |
#' |yellow_card      | int  |
#' |red_card         | int  |
#' |game_id          | chr  |
#' |sog_against      | int  |
#' |ga               | int  |
#' |sv               | int  |
#' |goalie_mins      | chr  |
#' |game_date        | chr  |
#' |season           | chr  |
#' |season_type      | chr  |
#'
#' @export

#' @examples
#' try(field_hockey_player_box_score(2018))
#' @export

field_hockey_player_box_score <- function(year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  seasons <- create_season(year)
  sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/field_hockey_player_box_score/womens_player_box_score_",season,".csv",sep = "")) %>%
    lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
}

