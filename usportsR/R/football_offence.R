#' Get U SPORTS football offence stats
#'
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name       | Type |
#' |:-----------------|:-----|
#' |player            | chr  |
#' |c_a               | chr  |
#' |passing_yds       | int  |
#' |passing_lg        | int  |
#' |passing_td        | int  |
#' |passing_int       | int  |
#' |rtg               | lgl  |
#' |rushing_att       | int  |
#' |rushing_yds       | int  |
#' |rushing_avg       | dbl  |
#' |rushing_lg        | int  |
#' |rushing_td        | int  |
#' |receiving_no      | int  |
#' |receiving_yds     | int  |
#' |receiving_avg     | dbl  |
#' |receiving_lg      | int  |
#' |receiving_td      | int  |
#' |fumble_no         | int  |
#' |fumble_lost       | int  |
#' |team              | chr  |
#' |game_id           | chr  |
#' |season            | chr  |
#' |season_type       | chr  |
#'
#' @examples
#' try(football_offence(2018))
#' @export

football_offence <- function(year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  seasons <- create_season(year)
  sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/fball_offence/fball_offence_",season, ".csv")) %>%
    lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
}

