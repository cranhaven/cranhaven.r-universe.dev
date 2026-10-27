#' Get U SPORTS soccer play by play data
#'
#' @param gender Gender of athletes "m" or "w"
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name   | Type |
#' |:-------------|:-----|
#' |X             | int  |
#' |time          | chr  |
#' |event         | chr  |
#' |game_id       | chr  |
#' |away          | chr  |
#' |home          | chr  |
#' |season        | chr  |
#' |season_type   | chr  |
#'
#' @examples
#' \donttest{try(soccer_pbp("m",2018))}
#' @export

soccer_pbp <- function(gender,year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  if (gender == "m"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/msoc_pbp/msoc_pbp_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else if(gender == "w"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/wsoc_pbp/wsoc_pbp_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else{
    stop("Invalid input: 'gender' must be one of 'm' or 'w'")
  }
}

