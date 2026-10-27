#' Get U SPORTS hockey team box scores
#'
#' @param gender Gender of athletes "m" or "w"
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name    | Type |
#' |:--------------|:-----|
#' |team           | chr  |
#' |goals_p1       | int  |
#' |goals_p2       | int  |
#' |goals_p3       | int  |
#' |goals_pot      | int  |
#' |goals_p2ot     | int  |
#' |goals_p3ot     | int  |
#' |goals_p4ot     | lgl  |
#' |goals_total    | int  |
#' |shots_1        | int  |
#' |shots_2        | int  |
#' |shots_3        | int  |
#' |shots_total    | int  |
#' |shots_ot       | int  |
#' |shots_2ot      | int  |
#' |shots_3ot      | int  |
#' |shots_4ot      | lgl  |
#' |game_id        | chr  |
#' |season         | chr  |
#' |season_type    | chr  |
#'
#' @examples
#' try(hockey_team_box_score("m",2018))
#' @export

hockey_team_box_score <- function(gender,year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  if (gender == "m"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/hockey_team_box_score/mens_team_box_score_",season,".csv",sep = "")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else if(gender == "w"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/hockey_team_box_score/womens_team_box_score_",season,".csv",sep = "")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else{
    stop("Invalid input: 'gender' must be one of 'm' or 'w'")
  }
}

