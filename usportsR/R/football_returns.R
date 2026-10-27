#' Get U SPORTS football returns stats
#'
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name                     | Type |
#' |:-------------------------------|:-----|
#' |player                          | chr  |
#' |punting_returns_no              | int  |
#' |punting_returns_yds             | int  |
#' |punting_returns_avg             | dbl  |
#' |punting_returns_lg              | int  |
#' |punting_returns_td              | int  |
#' |kickoff_returns_no              | int  |
#' |kickoff_returns_yds             | int  |
#' |kickoff_returns_avg             | dbl  |
#' |kickoff_returns_lg              | int  |
#' |kickoff_returns_td              | int  |
#' |interception_returns_no         | int  |
#' |interception_returns_yds        | int  |
#' |interception_returns_avg        | dbl  |
#' |interception_returns_lg         | int  |
#' |interception_returns_td         | int  |
#' |team                            | chr  |
#' |game_id                         | chr  |
#' |season                          | chr  |
#' |season_type                     | chr  |
#'
#' @examples
#' try(football_returns(2018))
#' @export

football_returns <- function(year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  seasons <- create_season(year)
  sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/fball_returns/fball_returns_",season, ".csv")) %>%
    lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
}

