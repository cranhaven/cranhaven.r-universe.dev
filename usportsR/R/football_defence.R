#' Get U SPORTS football defence stats
#'
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name   | Type |
#' |:-------------|:-----|
#' |number        | chr  |
#' |player        | chr  |
#' |solo          | int  |
#' |ast           | int  |
#' |total         | dbl  |
#' |sacks         | dbl  |
#' |sacks_yds     | int  |
#' |tfl           | dbl  |
#' |tfl_yds       | int  |
#' |ff            | int  |
#' |fr            | int  |
#' |fr_yds        | int  |
#' |int           | int  |
#' |int_yds       | int  |
#' |br_up         | int  |
#' |blks          | int  |
#' |qbh           | int  |
#' |team          | chr  |
#' |game_id       | chr  |
#' |season        | chr  |
#' |season_type   | chr  |
#'
#' @examples
#' try(football_defence(2018))
#' @export

football_defence <- function(year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  seasons <- create_season(year)
  sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/fball_defence/fball_defence_",season, ".csv")) %>%
    lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
}

