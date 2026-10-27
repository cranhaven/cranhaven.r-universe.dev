#' Get U SPORTS volleyball player box scores
#'
#' @param gender Gender of athletes "m" or "w"
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name      | Type |
#' |:----------------|:-----|
#' |player_number    | chr  |
#' |player           | chr  |
#' |sp               | chr  |
#' |k                | int  |
#' |e                | int  |
#' |ta               | int  |
#' |k_percentage     | dbl  |
#' |a                | int  |
#' |sa               | int  |
#' |se               | int  |
#' |re               | int  |
#' |digs             | int  |
#' |bs               | int  |
#' |ba               | int  |
#' |be               | int  |
#' |bhe              | int  |
#' |pts              | dbl  |
#' |team_name        | chr  |
#' |player_links     | chr  |
#' |game_id          | chr  |
#' |date_time        | chr  |
#' |start            | lgl  |
#' |season           | chr  |
#' |season_type      | chr  |
#'
#' @examples
#' try(volleyball_player_box_score("m",2018))
#' @export

volleyball_player_box_score <- function(gender, year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  if (gender == "m"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/mvball_player_box/mvball_player_box_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else if(gender == "w"){
    seasons <- create_season(year)
    sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/wvball_player_box/wvball_player_box_",season, ".csv")) %>%
      lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
  }else{
    stop("Invalid input: 'gender' must be one of 'm' or 'w'")
  }
}

