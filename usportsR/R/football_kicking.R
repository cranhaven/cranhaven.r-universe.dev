#' Get U SPORTS football kicking stats
#'
#' @param year The season(s) of interest.
#'
#' @returns
#' |Column Name      | Type |
#' |:----------------|:-----|
#' |player           | chr  |
#' |punting_no       | int  |
#' |punting_yds      | int  |
#' |punting_avg      | dbl  |
#' |punting_lg       | int  |
#' |punting_tb       | int  |
#' |in_20            | int  |
#' |kicking_fga      | int  |
#' |kicking_fgm      | int  |
#' |kicking_lg       | int  |
#' |kicking_xpa      | int  |
#' |kicking_xpm      | int  |
#' |kicking_rg       | int  |
#' |kicking_pts      | int  |
#' |kickoffs_no      | int  |
#' |kickoffs_yds     | int  |
#' |kickoffs_avg     | dbl  |
#' |kickoffs_tb      | int  |
#' |kickoffs_ob      | int  |
#' |team             | chr  |
#' |game_id          | chr  |
#' |season           | chr  |
#' |season_type      | chr  |
#'
#' @examples
#' try(football_kicking(2018))
#' @export

football_kicking <- function(year = c()) {
  if(length(year) < 1){
    stop("Argument `year` is missing, with no default. Please select a season")
  }
  seasons <- create_season(year)
  sapply(seasons, function(season) paste0("https://github.com/uwaggs/usports-data/releases/download/fball_kicking/fball_kicking_",season, ".csv")) %>%
    lapply(read_csv) %>% dplyr::bind_rows() %>% tibble::tibble()
}

