#' Get U SPORTS basketball team box scores
#'
#' @param gender Gender of athletes "m" or "w"
#' @param year The season(s) of interest.
#' @returns
#' |Column Name                          | Type |
#' |:------------------------------------|:-----|
#' |field_goals_made                     | int  |
#' |field_goals_attempted                | int  |
#' |field_goal_percentage                | chr  |
#' |three_point_field_goals_made         | int  |
#' |three_point_field_goals_attempted    | int  |
#' |three_point_field_goal_percentage    | chr  |
#' |free_throws_made                     | int  |
#' |free_throws_attempted                | int  |
#' |free_throw_percentage                | chr  |
#' |rebounds                             | int  |
#' |assists                              | int  |
#' |turnovers                            | int  |
#' |points_off_turnovers                 | int  |
#' |second_chance_points                 | int  |
#' |points_in_the_paint                  | int  |
#' |fastbreak_points                     | int  |
#' |bench_points                         | int  |
#' |largest_lead                         | int  |
#' |trends                               | chr  |
#' |team_name                            | chr  |
#' |game_id                              | chr  |
#' |date                                 | chr  |
#' |q1                                   | int  |
#' |q2                                   | int  |
#' |q3                                   | int  |
#' |q4                                   | int  |
#' |total                                | int  |
#' |time_of_largest_lead                 | chr  |
#' |ot                                   | int  |
#' |x2ot                                 | int  |
#' |x3ot                                 | int  |
#' |season                               | chr  |
#' |season_type                          | chr  |
#'
#' @examples
#' try(basketball_team_box_score("m",2018))
#' @export

basketball_team_box_score <- function(gender, year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  if (gender == "m"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/mbkb_team_box/mbkb_team_box_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else if(gender == "w"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/wbkb_team_box/wbkb_team_box_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else{
    stop("Invalid input: 'gender' must be one of 'm' or 'w'")
  }
}

