#' Get U SPORTS field hockey pbp
#'
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name   | Type |
#' |:-------------|:-----|
#' |time          | chr  |
#' |event         | chr  |
#' |quarters      | int  |
#' |game_id       | chr  |
#' |season        | chr  |
#' |season_type   | chr  |
#'
#' @examples
#' \donttest{try(field_hockey_pbp(2018))}
#' @export

field_hockey_pbp <- function(year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  seasons <- create_season(year)
  sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/field_hockey_pbp/womens_pbp_",season,".csv",sep = "")) %>%
    lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
}

